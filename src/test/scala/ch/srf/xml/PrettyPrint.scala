package ch.srf.xml

import scala.xml.{Node, PrettyPrinter}

object PrettyPrint {

  // PrettyPrinter is not thread-safe!
  private def prettyPrinter = new PrettyPrinter(80, 2)

  def apply(node: Node): String = prettyPrinter.format(node)

}
