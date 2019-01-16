package ch.srf.xml

import scalaz.Monoid

import scala.xml.{Attribute, Elem, Null, Text}

private[xml] final case class ElemValue(attributes: Map[String, String],
                                        elements: List[Elem],
                                        text: Option[String]) {

  def toElem(name: String): Elem = {
    val elem = <dummy/>.copy(label = name)
    elements.foldRight(
      attributes.foldRight(
        text.map(t => elem.copy(child = elem.child :+ Text(t))).getOrElse(elem)
      ) { case ((n, value), parent) => parent % Attribute(None, n, Text(value), Null) }
    ) { case (e, parent) => parent.copy(child = e +: parent.child) }
  }

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
