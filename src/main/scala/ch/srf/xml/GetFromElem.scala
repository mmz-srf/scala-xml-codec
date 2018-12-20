package ch.srf.xml

import ch.srf.xml.scalazext.syntax._
import scalaz.Id.Id
import scalaz.syntax.all._
import scalaz.syntax.std.list._
import scalaz.syntax.std.option._
import scalaz.{@@, Applicative, Monad, NonEmptyList, Tag}

import scala.xml.Elem

private[xml] sealed trait GetFromElem[F[_], D, C[_], X] {

  def apply(e: Elem, id: D, filter: X => Result[F, Boolean]): Result[F, C[X]]

}

private[xml] object GetFromElem {

  private def apply[F[_], D, C[_], X](f: (Elem, D, X => Result[F, Boolean]) => Result[F, C[X]])
  : GetFromElem[F, D, C, X] =
    new GetFromElem[F, D, C, X] {
      override def apply(e: Elem, id: D, filter: X => Result[F, Boolean]): Result[F,  C[X]] =
        f(e, id, filter)
    }

  private def flatMapResult[F[_]:Monad, A, B](result: Result[F, A])(f: A => Result[F, B]): Result[F, B] =
    result.monadic.flatMap(f(_).monadic).applicative

  /* ----- Attributes ----- */

  private def getAttribute[F[_]:Applicative](e: Elem,
                                             name: String,
                                             filter: String @@ AttrValue => Result[F, Boolean])
  : Result[F, Option[String @@ AttrValue]] =
    e.attributes.asAttrMap.get(name).map(Tag.of[AttrValue](_)).filterM(filter)

  implicit def attrInstance[F[_]:Monad]: GetFromElem[F, String, Id, String @@ AttrValue] =
    apply[F, String, Id, String @@ AttrValue]((elem, name, filter) =>
      flatMapResult(getAttribute(elem, name, filter))(
        o => Result.fromDisjunction(o.\/>(s"Attribute '$name' missing").point[F], name))
    )

  implicit def attrOptionInstance[F[_]:Applicative]: GetFromElem[F, String, Option, String @@ AttrValue] =
    apply((elem, name, filter) => getAttribute(elem, name, filter))

  /* ----- Text ----- */

  private def nonEmptyTextValue[
  F[_]: Applicative](e: Elem, filter: String @@ NonEmptyTextValue => Result[F, Boolean])
  : Result[F, Option[String @@ NonEmptyTextValue]] =
    Option(e.text).filter(!_.isEmpty).map(Tag.of[NonEmptyTextValue](_)).filterM(filter)

  implicit def nonEmptyTextInstance[F[_]:Monad]: GetFromElem[F, Unit, Id, String @@ NonEmptyTextValue] =
    apply[F, Unit, Id, String @@ NonEmptyTextValue]((elem, _, filter) =>
      flatMapResult(nonEmptyTextValue(elem, filter))(
        o => Result.fromDisjunction(o.\/>("Text must not be empty").point[F], Descriptor.text.name))
    )

  implicit def textInstance[F[_]:Monad]: GetFromElem[F, Unit, Id, String @@ TextValue] =
    apply[F, Unit, Id, String @@ TextValue]((elem, _, filter) =>
      flatMapResult(
        Option(Tag.of[TextValue](elem.text)).filterM(filter))(
        o => Result.fromDisjunction(o.\/>("No matching text found").point[F], Descriptor.text.name))
    )

  implicit def nonEmptyTextOptionInstance[F[_]
  : Applicative]: GetFromElem[F, Unit, Option, String @@ NonEmptyTextValue] =
    apply((elem, _, filter) => nonEmptyTextValue(elem, filter))

  /* ----- Elements ----- */

  private def elems[F[_]: Applicative](parent: Elem,
                                       name: String,
                                       filter: Elem => Result[F, Boolean]): Result[F, List[Elem]] =
    (parent \ name).toList.collect { case e: Elem => e }.filterM(filter)

  implicit def elemInstance[F[_]:Monad]: GetFromElem[F, String, Id, Elem] =
    apply[F, String, Id, Elem]((elem, name, filter) =>
      flatMapResult(elems(elem, name, filter)) {
        case h :: Nil => Result.success[F, Elem](h)
        case l => Result
          .error[F, Elem](Path.single(name), s"Exactly one element <$name> expected, found ${l.size}")
      }
    )

  implicit def elemOptionInstance[F[_]:Monad, CS]: GetFromElem[F, String, Option, Elem] =
    apply((elem, name, filter) =>
      flatMapResult(elems(elem, name, filter)) {
        case Nil => Result.success[F, Option[Elem]](None)
        case List(h) => Result.success[F, Option[Elem]](Some(h))
        case l => Result
          .error[F, Option[Elem]](Path.single(name), s"At most one element <$name> expected, found ${l.size}")
      }
    )

  implicit def elemListInstance[F[_]: Applicative, CS]: GetFromElem[F, String, List, Elem] =
    apply((elem, name, filter) => elems(elem, name, filter))

  implicit def elemNelInstance[F[_]:Monad, CS]: GetFromElem[F, String, NonEmptyList, Elem] =
    apply((elem, name, filter) =>
      flatMapResult(elems(elem, name, filter))(
        l => Result.fromDisjunction(l.toNel.\/>(s"At least one element <$name> expected").point[F], name))
    )

}