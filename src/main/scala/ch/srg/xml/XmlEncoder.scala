package ch.srg.xml

import ch.srg.xml.util.{CompactHList, Flatten}
import scalaz.syntax.contravariant._
import scalaz.syntax.traverse._
import scalaz.{@@, Monad, Tag, Traverse}

import scala.xml.Elem

sealed abstract class XmlEncoder[F[_]:Monad, D, X, A] {
  outer =>

  def descriptor: Descriptor[D]

  def encoder: Encoder[F, X, A]

  def as[B](implicit enc: Encoder[F, A, B]): XmlEncoder[F, D, X, B] =
    this ~ enc

  def ~[B](enc: Encoder[F, A, B]): XmlEncoder[F, D, X, B] =
    new XmlEncoder[F, D, X, B] {

      override def descriptor: Descriptor[D] =
        outer.descriptor

      override def encoder: Encoder[F, X, B] =
        outer.encoder ~ enc
    }

  def skip[B](implicit ev: Flatten[A, B]): XmlEncoder[F, D, X, B] =
    this ~ Encoder.fromFunction(ev.from)


  def encode(a: A): F[X] =
    encoder.encode(a)
}

object XmlEncoder {

  def collection[F[_]:Monad, C[_]:Traverse, D, X, A](enc: XmlEncoder[F, D, X, A]): XmlEncoder[F, D, C[X], C[A]] =
    new XmlEncoder[F, D, C[X], C[A]] {

      override def descriptor: Descriptor[D] =
        enc.descriptor

      override def encoder: Encoder[F, C[X], C[A]] =
        Encoder(_.traverse(enc.encoder.encode))

    }


  private def textEncoder[F[_]:Monad, T]: XmlEncoder[F, Unit, String @@ T, String] =
    new XmlEncoder[F, Unit, String @@ T, String] {

      override def encoder: Encoder[F, String @@ T, String] =
        Encoder.fromFunction(v => Tag.of[T](v))

      override def descriptor: Descriptor[Unit] =
        Descriptor.text

    }

  def text[F[_]:Monad]: XmlEncoder[F, Unit, String @@ TextValue, String] =
    textEncoder[F, TextValue]

  def nonEmptyText[F[_]:Monad]: XmlEncoder[F, Unit, String @@ NonEmptyTextValue, String] =
    textEncoder[F, NonEmptyTextValue]

  def attr[F[_]:Monad](name: String): XmlEncoder[F, String, String @@ AttrValue, String] =
    new XmlEncoder[F, String, String @@ AttrValue, String] {
      outer =>

      override def encoder: Encoder[F, String @@ AttrValue, String] =
        Encoder.fromFunction(v => Tag.of[AttrValue](v))

      override def descriptor: Descriptor[String] =
        Descriptor.attr(name)

    }

  def elem[F[_]:Monad, CS, C, A](name: String, children: CS)
                                (implicit
                                 hListEncoder: HListEncoder[F, CS, C],
                                 compact: CompactHList[C, A]): XmlEncoder[F, String, Elem, A] =
    new XmlEncoder[F, String, Elem, A] {

      private def compactEncoder: Encoder[F, C, A] =
        Encoder.fromFunction[F, C, A](compact.from)

      def encoder: Encoder[F, Elem, A] =
        hListEncoder.apply(children).contramap[C]((<dummy/>.copy(label = name), _)) ~ compactEncoder

      override def descriptor: Descriptor[String] =
        Descriptor.elem(name)

    }

}