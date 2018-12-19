package ch.srf.xml

import ch.srf.xml.util.{CompactHList, Flatten}
import scalaz.syntax.contravariant._
import scalaz.syntax.traverse._
import scalaz.{@@, Monad, Tag, Traverse}

import scala.xml.Elem

final case class XmlEncoder[F[_]:Monad, D, X, A](descriptor: Descriptor[D],
                                                 encoder: Encoder[F, X, A]) {

  def as[B](implicit enc: Encoder[F, A, B]): XmlEncoder[F, D, X, B] =
    this ~ enc

  def ~[B](enc: Encoder[F, A, B]): XmlEncoder[F, D, X, B] =
    XmlEncoder[F, D, X, B](descriptor, encoder ~ enc)

  def skip[B](implicit ev: Flatten[A, B]): XmlEncoder[F, D, X, B] =
    this ~ Encoder.fromFunction(ev.from)

  def encode(a: A): F[X] =
    encoder.encode(a)
}

object XmlEncoder {

  def collection[F[_]:Monad, C[_]:Traverse, D, X, A](enc: XmlEncoder[F, D, X, A]): XmlEncoder[F, D, C[X], C[A]] =
    XmlEncoder[F, D, C[X], C[A]](enc.descriptor,  Encoder(_.traverse(enc.encoder.encode)))

  private def textEncoder[F[_]:Monad, T]: XmlEncoder[F, Unit, String @@ T, String] =
    XmlEncoder[F, Unit, String @@ T, String](
      Descriptor.text,
      Encoder.fromFunction(v => Tag.of[T](v))
    )

  def text[F[_]:Monad]: XmlEncoder[F, Unit, String @@ TextValue, String] =
    textEncoder[F, TextValue]

  def nonEmptyText[F[_]:Monad]: XmlEncoder[F, Unit, String @@ NonEmptyTextValue, String] =
    textEncoder[F, NonEmptyTextValue]

  def attr[F[_]:Monad](name: String): XmlEncoder[F, String, String @@ AttrValue, String] =
    XmlEncoder[F, String, String @@ AttrValue, String](
      Descriptor.attr(name),
      Encoder.fromFunction(v => Tag.of[AttrValue](v))
    )

  def elem[F[_]:Monad, CS, C, A](name: String, children: CS)
                                (implicit
                                 hListEncoder: HListEncoder[F, CS, C],
                                 compact: CompactHList[C, A]): XmlEncoder[F, String, Elem, A] = {

    def compactEncoder: Encoder[F, C, A] =
      Encoder.fromFunction[F, C, A](compact.from)

    new XmlEncoder[F, String, Elem, A](
      Descriptor.elem(name),
      hListEncoder.apply(children).contramap[C]((<dummy/>.copy(label = name), _)) ~ compactEncoder
    )

  }

}