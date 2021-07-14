package ch.srf.xml

import scalaz.syntax.either._
import scalaz.syntax.std.list._
import scalaz.syntax.std.option._
import scalaz.{@@, NonEmptyList, Tag, \/}

import scala.xml.Elem

private[xml] sealed trait GetFromElem[D, X] {

  def apply(e: Elem, id: D): String \/ X

}

private[xml] object GetFromElem {

  private def apply[D, X](f: (Elem, D) => String \/ X): GetFromElem[D, X] =
    new GetFromElem[D, X] {
      override def apply(e: Elem, id: D): String \/ X =
        f(e, id)
    }

  /* ----- Attributes ----- */

  def getAttribute(e: Elem, name: String): Option[String @@ AttrValue] =
    e.attributes.asAttrMap.get(name).map(Tag.of[AttrValue](_))

  implicit lazy val attrInstance: GetFromElem[String, String @@ AttrValue] =
    apply((elem, name) => getAttribute(elem, name).\/>(s"Attribute '$name' missing"))

  implicit lazy val attrOptionInstance: GetFromElem[String, Option[String @@ AttrValue]] =
    apply((elem, name) => getAttribute(elem, name).right)

  /* ----- Text ----- */

  def nonEmptyTextValue(e: Elem): Option[String @@ NonEmptyTextValue] = {
    Option(e.text).filter(!_.isEmpty).map(Tag.of[NonEmptyTextValue](_))
  }

  implicit lazy val nonEmptyTextInstance: GetFromElem[Unit, String @@ NonEmptyTextValue] =
    apply((elem, _) => nonEmptyTextValue(elem).\/>("Text must not be empty"))

  implicit lazy val textInstance: GetFromElem[Unit, String @@ TextValue] =
    apply((elem, _) => Tag.of[TextValue](elem.text).right)

  implicit lazy val nonEmptyTextOptionInstance: GetFromElem[Unit, Option[String @@ NonEmptyTextValue]] =
    apply((elem, _) => nonEmptyTextValue(elem).right)

  /* ----- Elements ----- */

  def elems(parent: Elem, name: String): List[Elem] =
    (parent \ name).toList.collect { case e: Elem => e }

  implicit lazy val elemInstance: GetFromElem[String, Elem] =
    apply((elem, name) => elems(elem, name) match {
      case h :: Nil => h.right
      case l => s"Exactly one element <$name> expected, found ${l.size.toString}".left
    })

  implicit def elemOptionInstance[CS]: GetFromElem[String, Option[Elem]] =
    apply((elem, name) => elems(elem, name) match {
      case Nil => None.right
      case List(h) => Some(h).right
      case l => s"At most one element <$name> expected, found ${l.size.toString}".left
    })

  implicit def elemListInstance[CS]: GetFromElem[String, List[Elem]] =
    apply((elem, name) => elems(elem, name).right)

  implicit def elemNelInstance[CS]: GetFromElem[String, NonEmptyList[Elem]] =
    apply((elem, name) => elems(elem, name).toNel.\/>(s"At least one element <$name> expected"))

}
