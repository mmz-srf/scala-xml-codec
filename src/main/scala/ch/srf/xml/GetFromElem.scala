package ch.srf.xml

import ch.srf.xml.scalazext.syntax._
import scalaz.syntax.applicative._
import scalaz.syntax.either._
import scalaz.syntax.std.boolean._
import scalaz.syntax.std.list._
import scalaz.syntax.std.option._
import scalaz.{@@, Applicative, Functor, NonEmptyList, Tag, \/}

import scala.xml.Elem

private[xml] sealed trait GetFromElem[F[_], D, X] {

  def apply(e: Elem, id: D, filter: X => F[Boolean]): F[String \/ X]

}

private[xml] object GetFromElem {
/*
  private def matching[F[_]:Functor, A, B](a: A,
                                           filter: A => F[Boolean],
                                           ifNotMatching: => String \/ B)
                                          (f: A => String \/ B): F[String \/ B] =
    filter(a).map(_.fold(f(a), ifNotMatching))
*/
  private def apply[F[_], D, X](f: (Elem, D, X => F[Boolean]) => F[String \/ X]): GetFromElem[F, D, X] =
    new GetFromElem[F, D, X] {
      override def apply(e: Elem, id: D, filter: X => F[Boolean]): F[String \/ X] =
        f(e, id, filter)
    }

  /* ----- Attributes ----- */

  def getAttribute[F[_]:Applicative](e: Elem, name: String): Option[String @@ AttrValue] =
    e.attributes.asAttrMap.get(name).map(Tag.of[AttrValue](_))

  implicit def attrInstance[F[_]:Applicative]: GetFromElem[F, String, String @@ AttrValue] =
    apply((elem, name, filter) =>
      getAttribute(elem, name).filterM(filter).map(_.\/>(s"Attribute '$name' missing")))

  implicit def attrOptionInstance[F[_]:Applicative]: GetFromElem[F, String, Option[String @@ AttrValue]] =
    apply { (elem, name, filter) =>
      val attr = getAttribute(elem, name)
      filter(attr).map(_ => attr.right)
    }

  /* ----- Text ----- */

  def nonEmptyTextValue[F[_]:Applicative](e: Elem): Option[String @@ NonEmptyTextValue] =
    Option(e.text).filter(!_.isEmpty).map(Tag.of[NonEmptyTextValue](_))

  implicit def nonEmptyTextInstance[F[_]: Applicative]: GetFromElem[F, Unit, String @@ NonEmptyTextValue] =
    apply((elem, _, filter) =>
      nonEmptyTextValue(elem).filterM(filter).map(_.\/>("Text must not be empty")))

  implicit def textInstance[F[_]: Applicative]: GetFromElem[F, Unit, String @@ TextValue] =
    apply((elem, _, filter) =>
      Option(Tag.of[TextValue](elem.text)).filterM(filter).map(_.\/>("No matching text found")))

  implicit def nonEmptyTextOptionInstance[F[_]
  : Applicative]: GetFromElem[F, Unit, Option[String @@ NonEmptyTextValue]] =
    apply { (elem, _, filter) =>
      val text = nonEmptyTextValue(elem)
      filter(text).map(_.fold(text.right, None.right))
    }

  /* ----- Elements ----- */

  def elems[F[_]: Applicative](parent: Elem, name: String): List[Elem] =
    (parent \ name).toList.collect { case e: Elem => e }

  implicit def elemInstance[F[_]: Applicative]: GetFromElem[F, String, Elem] =
    apply[F, String, Elem]((elem, name, filter) =>
      elems(elem, name).filterM(filter).map {
        case h :: Nil => h.right
        case l => s"Exactly one element <$name> expected, found ${l.size}".left
      }
    )

  implicit def elemOptionInstance[F[_]: Applicative, CS]: GetFromElem[F, String, Option[Elem]] =
    apply { (elem, name, filter) =>
      lazy val none = Option.empty[Elem].right[String]
      elems(elem, name) match {
        case Nil => none.point[F]
        case List(h) => filter(Some(h)).map(_.fold(Some(h).right,none))
        case l => s"At most one element <$name> expected, found ${l.size}".left.point[F]
      }
    }

  implicit def elemListInstance[F[_]: Applicative, CS]: GetFromElem[F, String, List[Elem]] =
    apply { (elem, name, filter) =>
      val matching = elems(elem, name)
      filter(matching).map(_.fold(matching.right, List.empty[Elem].right))
    }

  implicit def elemNelInstance[F[_]: Applicative, CS]: GetFromElem[F, String, NonEmptyList[Elem]] =
    apply { (elem, name, filter) =>
      lazy val error =  s"At least one element <$name> expected"
      elems(elem, name).toNel match {
        case None => error.left.point[F]
        case Some(nel) => filter(nel).map(_.fold(nel.right, error.left))
      }
    }

}