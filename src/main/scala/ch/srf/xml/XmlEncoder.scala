package ch.srf.xml

import cats.Contravariant
import cats.Functor
import cats.syntax.all._
import cats.{Monad, Traverse}
import ch.srf.xml.util.{CompactHList, Flatten}
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

  final def xFunctor: XmlEncoder.XFunctor[F, D, A, X] = new XmlEncoder.XFunctor(this)
}

object XmlEncoder {

  sealed class XFunctor[F[_], D, A, X](val xmlEncoder: XmlEncoder[F, D, X, A])


  object XFunctor {
    implicit def functor[F[_]: Monad, D, A]: Functor[XFunctor[F, D, A, *]] = new Functor[XFunctor[F, D, A, *]] {
      override def map[B, C](fa: XFunctor[F,D,A,B])(f: B => C): XFunctor[F,D,A,C] = new XFunctor[F,D,A,C](
        new XmlEncoder[F,D,C,A] {
          override def descriptor: Descriptor[D] = fa.xmlEncoder.descriptor
          override def encoder: Encoder[F,C,A] = fa.xmlEncoder.encoder.xFunctor.map(f).encoder
        }
      )
    }
  }
  implicit def contra[F[_]: Monad, D, X]: Contravariant[XmlEncoder[F, D, X, *]] = new Contravariant[XmlEncoder[F, D, X, *]] {
    override def contramap[A, B](fa: XmlEncoder[F,D,X,A])(f: B => A): XmlEncoder[F,D,X,B] =
      fa ~ Encoder.fromFunction(f)
  }

  def collection[F[_]:Monad, C[_]:Traverse, D, X, A](enc: XmlEncoder[F, D, X, A]): XmlEncoder[F, D, C[X], C[A]] =
    new XmlEncoder[F, D, C[X], C[A]] {

      override def descriptor: Descriptor[D] =
        enc.descriptor

      override def encoder: Encoder[F, C[X], C[A]] =
        Encoder(_.traverse(enc.encoder.encode))

    }


  private def textEncoder[F[_]:Monad]: XmlEncoder[F, Unit, String, String] =
    new XmlEncoder[F, Unit, String, String] {

      override def encoder: Encoder[F, String, String] =
        Encoder.id

      override def descriptor: Descriptor[Unit] =
        Descriptor.text

    }

  def text[F[_]:Monad]: XmlEncoder[F, Unit, TextValue, String] =
    textEncoder[F].xFunctor.map(TextValue.apply).xmlEncoder

  def nonEmptyText[F[_]:Monad]: XmlEncoder[F, Unit, NonEmptyTextValue, String] =
    textEncoder[F].xFunctor.map(NonEmptyTextValue.apply).xmlEncoder

  def attr[F[_]:Monad](name: String): XmlEncoder[F, String, AttrValue, String] =
    new XmlEncoder[F, String, AttrValue, String] {
      outer =>

      override def encoder: Encoder[F, AttrValue, String] =
        Encoder.fromFunction(AttrValue(_))

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
