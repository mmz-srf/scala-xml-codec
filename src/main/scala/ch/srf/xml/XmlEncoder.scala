package ch.srf.xml

import ch.srf.xml.util.{CompactHList, Flatten}
import scalaz.syntax.contravariant._
import scalaz.syntax.traverse._
import scalaz.Id.Id
import scalaz.{@@, Monad, Tag, Traverse}

import scala.xml.Elem

final case class XmlEncoder[F[_]:Monad, D, C[_], X, A](descriptor: Descriptor[D],
                                                       encoder: Encoder[F, C[X], C[A]]) {

  def as[B](implicit enc: Encoder[F, C[A], C[B]]): XmlEncoder[F, D, C, X, B] =
    this ~ enc

  def ~[B](enc: Encoder[F, C[A], C[B]]): XmlEncoder[F, D, C, X, B] =
    XmlEncoder[F, D, C, X, B](descriptor, encoder ~ enc)

  def skip[B](implicit ev: Flatten[C[A], C[B]]): XmlEncoder[F, D, C, X, B] =
    this ~ Encoder.fromFunction(ev.from)


  def encode(a: C[A]): F[C[X]] =
    encoder.encode(a)
}

object XmlEncoder {

  def collection[F[_]:Monad, C[_]:Traverse, D, X, A](enc: XmlEncoder[F, D, Id, X, A]): XmlEncoder[F, D, C, X, A] =
    XmlEncoder[F, D, C, X, A](enc.descriptor,  Encoder(_.traverse(enc.encoder.encode)))

  private def textEncoder[F[_]:Monad, T]: XmlEncoder[F, Unit, Id, String @@ T, String] =
    XmlEncoder[F, Unit, Id, String @@ T, String](
      Descriptor.text,
      Encoder.fromFunction(v => Tag.of[T](v))
    )

  def text[F[_]:Monad]: XmlEncoder[F, Unit, Id, String @@ TextValue, String] =
    textEncoder[F, TextValue]

  def nonEmptyText[F[_]:Monad]: XmlEncoder[F, Unit, Id, String @@ NonEmptyTextValue, String] =
    textEncoder[F, NonEmptyTextValue]

  def attr[F[_]:Monad](name: String): XmlEncoder[F, String, Id, String @@ AttrValue, String] =
    XmlEncoder[F, String, Id, String @@ AttrValue, String](
      Descriptor.attr(name),
      Encoder.fromFunction(v => Tag.of[AttrValue](v))
    )

  def elem[F[_]:Monad, CS, C, A](name: String, children: CS)
                                (implicit
                                 hListEncoder: HListEncoder[F, CS, C],
                                 compact: CompactHList[C, A]): XmlEncoder[F, String, Id, Elem, A] = {

    def compactEncoder: Encoder[F, C, A] =
      Encoder.fromFunction[F, C, A](compact.from)

    new XmlEncoder[F, String, Id, Elem, A](
      Descriptor.elem(name),
      hListEncoder.apply(children).contramap[C]((<dummy/>.copy(label = name), _)) ~ compactEncoder
    )

  }

}