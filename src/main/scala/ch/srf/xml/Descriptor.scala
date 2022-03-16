package ch.srf.xml

import cats.kernel.Semigroup
import cats.syntax.all._

private[xml] final case class Descriptor[I](identifier: I, name: String)

private[xml] object Descriptor {

  def text: Descriptor[Unit] = Descriptor((), "<text>")

  def attr(attrName: String): Descriptor[String] = Descriptor(attrName, "@" + attrName)

  def elem(elemName: String): Descriptor[String] = Descriptor(elemName, elemName)

  def or[I : Semigroup](one: Descriptor[I], two: Descriptor[I], separator: I) = Descriptor(
    identifier = one.identifier |+| separator |+| two.identifier,
    name = one.name |+| "|" |+| two.name
  )
}
