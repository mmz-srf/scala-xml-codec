package ch.srf.xml

import scala.xml.{Node, PrettyPrinter}

object PrettyPrint {

  private lazy val prettyPrinter = new PrettyPrinter(80, 2)

  def apply(node: Node): String = prettyPrinter.format(node)

}
