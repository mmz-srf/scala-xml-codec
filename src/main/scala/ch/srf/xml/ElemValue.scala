package ch.srf.xml

import scalaz.Monoid

import scala.xml.{Attribute, Elem, Null, Text}

private[xml] final case class ElemValue(attributes: Map[String, String],
                                        elements: List[Elem],
                                        text: Option[String]) {

  def appendTo(elem: Elem): Elem =
    elements.foldLeft(
      attributes.foldLeft(
        text.map(t => elem.copy(child = elem.child :+ Text(t))).getOrElse(elem)
      ) { case (parent, (name, value)) => parent % Attribute(None, name, Text(value), Null) }
    ) { case (parent, e) => e.copy(child = parent +: e.child) }

  def append(other: ElemValue): ElemValue =
    ElemValue(
      attributes ++ other.attributes,
      elements ::: other.elements,
      (text, other.text) match {
        case (Some(a), Some(b)) => Some(a + b)
        case (Some(a), None) => Some(a)
        case (None, Some(b)) => Some(b)
        case (None, None) => None
      }
    )
}

object ElemValue {

  lazy val empty: ElemValue = ElemValue(Map.empty, Nil, None)

  def fromElem(e: Elem): ElemValue =
    ElemValue(
      e.attributes.asAttrMap,
      (e \ "_").toList.collect { case e: Elem => e }.toList,
      Option(e.text).filter(_.nonEmpty)
    )

  implicit lazy val monoidInstance: Monoid[ElemValue] =
    Monoid.instance(
      _ append _,
      ElemValue.empty
    )

}
