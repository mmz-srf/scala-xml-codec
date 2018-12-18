package ch.srf.xml

import ch.srf.xml.scalazext.syntax._
import scalaz.Id.Id
import scalaz.syntax.either._
import scalaz.syntax.functor._
import scalaz.syntax.std.list._
import scalaz.syntax.std.option._
import scalaz.{@@, Applicative, NonEmptyList, Tag, \/}

import scala.xml.Elem

private[xml] sealed trait GetFromElem[F[_], D, C[_], X] {

  def apply(e: Elem, id: D, filter: X => F[Boolean]): F[String \/ C[X]]

}

private[xml] object GetFromElem {

  private def apply[F[_], D, C[_], X](f: (Elem, D, X => F[Boolean]) => F[String \/ C[X]]): GetFromElem[F, D, C, X] =
    new GetFromElem[F, D, C, X] {
      override def apply(e: Elem, id: D, filter: X => F[Boolean]): F[String \/ C[X]] =
        f(e, id, filter)
    }

  /* ----- Attributes ----- */

  def getAttribute[F[_]:Applicative](e: Elem, name: String,
                                     filter: String @@ AttrValue => F[Boolean]): F[Option[String @@ AttrValue]] =
    e.attributes.asAttrMap.get(name).map(Tag.of[AttrValue](_)).filterM(filter)

  implicit def attrInstance[F[_]:Applicative]: GetFromElem[F, String, Id, String @@ AttrValue] =
    apply[F, String, Id, String @@ AttrValue]((elem, name, filter) =>
      getAttribute(elem, name, filter).map(_.\/>(s"Attribute '$name' missing")))

  implicit def attrOptionInstance[F[_]:Applicative]: GetFromElem[F, String, Option, String @@ AttrValue] =
    apply((elem, name, filter) => getAttribute(elem, name, filter).map(_.right))

  /* ----- Text ----- */

  def nonEmptyTextValue[F[_]:Applicative](e: Elem, filter: String @@ NonEmptyTextValue => F[Boolean])
  : F[Option[String @@ NonEmptyTextValue]] =
    Option(e.text).filter(!_.isEmpty).map(Tag.of[NonEmptyTextValue](_)).filterM(filter)

  implicit def nonEmptyTextInstance[F[_]: Applicative]: GetFromElem[F, Unit, Id, String @@ NonEmptyTextValue] =
    apply[F, Unit, Id, String @@ NonEmptyTextValue]((elem, _, filter) =>
      nonEmptyTextValue(elem, filter).map(_.\/>("Text must not be empty")))

  implicit def textInstance[F[_]: Applicative]: GetFromElem[F, Unit, Id, String @@ TextValue] =
    apply[F, Unit, Id, String @@ TextValue]((elem, _, filter) =>
      Option(Tag.of[TextValue](elem.text)).filterM(filter).map(_.\/>("No matching text found")))

  implicit def nonEmptyTextOptionInstance[F[_]: Applicative]
  : GetFromElem[F, Unit, Option, String @@ NonEmptyTextValue] =
    apply((elem, _, filter) => nonEmptyTextValue(elem, filter).map(_.right))

  /* ----- Elements ----- */

  def elems[F[_]: Applicative](parent: Elem, name: String, filter: Elem => F[Boolean]): F[List[Elem]] =
    (parent \ name).toList.collect { case e: Elem => e }.filterM(filter)

  implicit def elemInstance[F[_]: Applicative]: GetFromElem[F, String, Id, Elem] =
    apply[F, String, Id, Elem]((elem, name, filter) =>
      elems(elem, name, filter).map {
        case h :: Nil => h.right
        case l => s"Exactly one element <$name> expected, found ${l.size}".left
      }
    )

  implicit def elemOptionInstance[F[_]: Applicative, CS]: GetFromElem[F, String, Option, Elem] =
    apply((elem, name, filter) =>
      elems(elem, name, filter) map {
        case Nil => None.right
        case List(h) => Some(h).right
        case l => s"At most one element <$name> expected, found ${l.size}".left
      }
    )

  implicit def elemListInstance[F[_]: Applicative, CS]: GetFromElem[F, String, List, Elem] =
    apply((elem, name, filter) => elems(elem, name, filter).map(_.right))

  implicit def elemNelInstance[F[_]: Applicative, CS]: GetFromElem[F, String, NonEmptyList, Elem] =
    apply((elem, name, filter) => elems(elem, name, filter).map(_.toNel.\/>(s"At least one element <$name> expected")))

}