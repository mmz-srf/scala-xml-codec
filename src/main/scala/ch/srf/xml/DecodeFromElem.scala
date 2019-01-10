package ch.srf.xml

import ch.srf.xml.scalazext.syntax._
import scalaz.Id.Id
import scalaz.std.list.listInstance
import scalaz.std.option.optionInstance
import scalaz.syntax.all._
import scalaz.syntax.std.boolean._
import scalaz.syntax.std.list._
import scalaz.syntax.std.option._
import scalaz.{Applicative, Monad, NonEmptyList, Traverse}

private[xml] sealed trait DecodeFromElem[F[_], C[_], X] {

  def apply[A](name: String,
               decode: X => Result[F, A],
               filter: X => Result[F, Boolean],
               e: ElemValue): Result[F, C[A]]

}

private[xml] object DecodeFromElem {

  private def flatMapResult[F[_]:Monad, A, B](result: Result[F, A])(f: A => Result[F, B]): Result[F, B] =
    result.monadic.flatMap(f(_).monadic).applicative

  /* ----- Attributes ----- */

  private def getAttribute[F[_]:Monad, A](name: String,
                                          decode: AttrValue => Result[F, A],
                                          filter: AttrValue => Result[F, Boolean],
                                          e: ElemValue): Result[F, Option[A]] =
    e.attributes.get(name).map(AttrValue.apply)
      .filterM(filter)
      .monadic
      .flatMap(_.traverse(decode).monadic)
      .applicative

  implicit def attrInstance[F[_]:Monad]: DecodeFromElem[F, Id, AttrValue] =
    new DecodeFromElem[F, Id, AttrValue] {
      override def apply[A](name: String,
                            decode: AttrValue => Result[F, A],
                            filter: AttrValue => Result[F, Boolean],
                            e: ElemValue): Result[F, A] =
        flatMapResult(getAttribute(name, decode, filter, e))(
          o => Result.fromDisjunction(o.\/>(s"Attribute '$name' missing").point[F]))
    }

  implicit def attrOptionInstance[F[_]:Monad]: DecodeFromElem[F, Option, AttrValue] =
    new DecodeFromElem[F, Option, AttrValue] {
      override def apply[A](name: String,
                            decode: AttrValue => Result[F, A],
                            filter: AttrValue => Result[F, Boolean],
                            e: ElemValue): Result[F, Option[A]] =
        getAttribute(name, decode, filter, e)
    }

  /* ----- Text ----- */

  private def nonEmptyTextValue[F[_]:Monad, A](decode: NonEmptyTextValue => Result[F, A],
                                               filter: NonEmptyTextValue => Result[F, Boolean],
                                               e: ElemValue): Result[F, Option[A]] =
    e.text.filter(!_.isEmpty).map(NonEmptyTextValue.apply)
      .filterM(filter)
      .monadic
      .flatMap(_.traverse(decode).monadic)
      .applicative

  implicit def nonEmptyTextInstance[F[_]:Monad]: DecodeFromElem[F, Id, NonEmptyTextValue] =
    new DecodeFromElem[F, Id, NonEmptyTextValue] {
      override def apply[A](name: String,
                            decode: NonEmptyTextValue => Result[F, A],
                            filter: NonEmptyTextValue => Result[F, Boolean],
                            e: ElemValue): Result[F, A] =
        flatMapResult(nonEmptyTextValue(decode, filter, e))(
          o => Result.fromDisjunction(o.\/>("Text must not be empty").point[F]))
    }

  implicit def textInstance[F[_]:Monad]: DecodeFromElem[F, Id, TextValue] =
    new DecodeFromElem[F, Id, TextValue] {
      override def apply[A](name: String,
                            decode: TextValue => Result[F, A],
                            filter: TextValue => Result[F, Boolean],
                            e: ElemValue): Result[F, A] =
        flatMapResult(
          e.text
            .map(TextValue.apply)
            .filterM(filter)
            .monadic
            .flatMap(_.traverse(decode).monadic)
            .applicative
        )(o => Result.fromDisjunction(o.\/>("No matching text found").point[F]))
    }

  implicit def nonEmptyTextOptionInstance[F[_]:Monad]: DecodeFromElem[F, Option, NonEmptyTextValue] =
    new DecodeFromElem[F, Option, NonEmptyTextValue] {
      override def apply[A](name: String,
                            decode: NonEmptyTextValue => Result[F, A],
                            filter: NonEmptyTextValue => Result[F, Boolean],
                            e: ElemValue): Result[F, Option[A]] =
        nonEmptyTextValue(decode, filter, e)
    }

  /* ----- Elements ----- */

  private type Positioned[A] = (A, Option[Int])

  private implicit class TraverseSyntax[G[_]:Traverse, X](val xs: G[Positioned[X]]) {

    def decodeTraverse[F[_]:Applicative, A](name: String, dec: X => Result[F, A]): Result[F, G[A]] =
      xs.traverse { case (e, pos) =>
        val result = dec(e)
        pos.fold(result.prependPath(name))(pos => result.prependPath(name + "[" + pos.toString + "]"))
      }

  }

  private def elems[F[_]:Monad, A](name: String,
                                   decode: ElemValue => Result[F, A],
                                   filter: ElemValue => Result[F, Boolean],
                                   parent: ElemValue): Result[F, List[A]] = {

    val elems = parent.elements.map(ElemValue.fromElem)

    (elems.size > 1)
      .fold(
        elems.zipWithIndex.map { case (e, pos) => (e, Some(pos)) },
        elems.map(e => (e, None))
      )
      .filterM { case (e, _) => filter(e) }
      .monadic
      .flatMap(_.decodeTraverse(name, decode).monadic)
      .applicative
  }

  implicit def elemInstance[F[_]:Monad]: DecodeFromElem[F, Id, ElemValue] =
    new DecodeFromElem[F, Id, ElemValue] {
      override def apply[A](name: String,
                            decode: ElemValue => Result[F, A],
                            filter: ElemValue => Result[F, Boolean],
                            e: ElemValue): Result[F, A] =
        flatMapResult(elems(name, decode, filter, e)) {
          case h :: Nil => Result.success[F, A](h)
          case l => Result.error[F, A](s"Exactly one element <$name> expected, found ${l.size}")
        }
    }

  implicit def elemOptionInstance[F[_]:Monad]: DecodeFromElem[F, Option, ElemValue] =
    new DecodeFromElem[F, Option, ElemValue] {
      override def apply[A](name: String,
                            decode: ElemValue => Result[F, A],
                            filter: ElemValue => Result[F, Boolean],
                            e: ElemValue): Result[F, Option[A]] =
        flatMapResult(elems(name, decode, filter, e)) {
          case Nil => Result.success[F, Option[A]](None)
          case List(h) => Result.success[F, Option[A]](Some(h))
          case l => Result
            .error[F, Option[A]](s"At most one element <$name> expected, found ${l.size}")
        }
    }

  implicit def elemListInstance[F[_]:Monad]: DecodeFromElem[F, List, ElemValue] =
    new DecodeFromElem[F, List, ElemValue] {
      override def apply[A](name: String,
                            decode: ElemValue => Result[F, A],
                            filter: ElemValue => Result[F, Boolean],
                            e: ElemValue): Result[F, List[A]] =
        elems(name, decode, filter, e)
    }

  implicit def elemNelInstance[F[_]:Monad]: DecodeFromElem[F, NonEmptyList, ElemValue] =
    new DecodeFromElem[F, NonEmptyList, ElemValue] {
      override def apply[A](name: String,
                            decode: ElemValue => Result[F, A],
                            filter: ElemValue => Result[F, Boolean],
                            e: ElemValue): Result[F, NonEmptyList[A]] =
        flatMapResult(elems(name, decode, filter, e))(
          l => Result.fromDisjunction(l.toNel.\/>(s"At least one element <$name> expected").point[F]))
    }

}