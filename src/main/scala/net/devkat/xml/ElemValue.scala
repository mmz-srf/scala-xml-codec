package net.devkat.xml

import scala.xml.Elem

final case class ElemValue(attrs: Map[String, String],
                           children: List[Elem],
                           text: Option[String])

object ElemValue {

  def fromElem(e: Elem): ElemValue =
    ElemValue(
      e.attributes.asAttrMap,
      (e \ "_").toList.collect { case e: Elem => e }.toList,
      Option(e.text).filter(_.nonEmpty)
    )

}
