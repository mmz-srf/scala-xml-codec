package ch.srf.xml

private[xml] final case class Descriptor[I](identifier: I, name: String)

private[xml] object Descriptor {

  def text: Descriptor[Unit] = Descriptor((), "<text>")

  def attr(attrName: String): Descriptor[String] = Descriptor(attrName, "@" + attrName)

  def elem(elemName: String): Descriptor[String] = Descriptor(elemName, elemName)

}