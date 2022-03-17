package ch.srf.xml

import cats.data.NonEmptyList
import cats.syntax.all._
import scala.xml.Elem

private[xml] sealed trait GetFromElem[D, X] {

  def apply(e: Elem, id: D): String Either X

}

private[xml] object GetFromElem {

  private def apply[D, X](f: (Elem, D) => String Either X): GetFromElem[D, X] =
    new GetFromElem[D, X] {
      override def apply(e: Elem, id: D): String Either X =
        f(e, id)
    }

  /* ----- Attributes ----- */

  def getAttribute(e: Elem, name: String): Option[AttrValue] =
    e.attributes.asAttrMap.get(name).map(AttrValue(_))

  implicit lazy val attrInstance: GetFromElem[String, AttrValue] =
    apply((elem, name) => getAttribute(elem, name).toRight(s"Attribute '$name' missing"))

  implicit lazy val attrOptionInstance: GetFromElem[String, Option[AttrValue]] =
    apply((elem, name) => getAttribute(elem, name).asRight)

  /* ----- Text ----- */

  def nonEmptyTextValue(e: Elem): Option[NonEmptyTextValue] = {
    Option(e.text).filter(!_.isEmpty).map(NonEmptyTextValue(_))
  }

  implicit lazy val nonEmptyTextInstance: GetFromElem[Unit, NonEmptyTextValue] =
    apply((elem, _) => nonEmptyTextValue(elem).toRight("Text must not be empty"))

  implicit lazy val textInstance: GetFromElem[Unit, TextValue] =
    apply((elem, _) => TextValue(elem.text).asRight)

  implicit lazy val nonEmptyTextOptionInstance: GetFromElem[Unit, Option[NonEmptyTextValue]] =
    apply((elem, _) => nonEmptyTextValue(elem).asRight)

  /* ----- Elements ----- */

  def elems(parent: Elem, name: String): List[Elem] =
    (parent \ name).toList.collect { case e: Elem => e }

  implicit lazy val elemInstance: GetFromElem[String, Elem] =
    apply((elem, name) => elems(elem, name) match {
      case h :: Nil => h.asRight
      case l => s"Exactly one element <$name> expected, found ${l.size.toString}".asLeft
    })

  implicit def elemOptionInstance[CS]: GetFromElem[String, Option[Elem]] =
    apply((elem, name) => elems(elem, name) match {
      case Nil => None.asRight
      case List(h) => Some(h).asRight
      case l => s"At most one element <$name> expected, found ${l.size.toString}".asLeft
    })

  implicit def elemListInstance[CS]: GetFromElem[String, List[Elem]] =
    apply((elem, name) => elems(elem, name).asRight)

  implicit def elemNelInstance[CS]: GetFromElem[String, NonEmptyList[Elem]] =
    apply((elem, name) => elems(elem, name).toNel.toRight(s"At least one element <$name> expected"))

}
