package ch.srf.xml

import ch.srf.xml.util.CompactHList
import scalaz.syntax.all._
import scalaz.{Applicative, Functor, Monad, Traverse}

import scala.xml.Elem

final case class XmlEncoder[F[_], X, A](name: String,
                                        enc: A => F[X]) {

  def as[B](implicit
            enc: Encoder[F, A, B],
            monadEv: Monad[F]): XmlEncoder[F, X, B] =
    this ~ enc

  def ~[B](e: Encoder[F, A, B])
          (implicit monadEv: Monad[F]): XmlEncoder[F, X, B] =
    XmlEncoder[F, X, B](name, (Encoder(enc) ~ e).encode)

  def encode(a: A)(implicit
                   ev: X =:= ElemValue,
                   functorEv: Functor[F]): F[Elem] =
    enc(a).map(ev(_).toElem(name))

}

object XmlEncoder {

  def collection[F[_]:Applicative, T[_]:Traverse, X, A](encoder: XmlEncoder[F, X, A])
                                                       (implicit append: AppendToElem[X]): TraverseEncoder[F, T, A] =

    TraverseEncoder.fromEncoder(encoder)

  def text[F[_]:Monad]: XmlEncoder[F, TextValue, String] =
    XmlEncoder("", TextValue(_).point[F])

  def nonEmptyText[F[_]:Monad]: XmlEncoder[F, NonEmptyTextValue, String] =
    XmlEncoder("", NonEmptyTextValue(_).point[F])

  def attr[F[_]:Monad](name: String): XmlEncoder[F, AttrValue, String] =
    XmlEncoder(name, AttrValue(_).point[F])

  def elem[F[_]:Monad, CS, C, A](name: String, children: CS)
                                (implicit
                                 hListEncoder: HListEncoder[F, CS, C],
                                 compact: CompactHList[C, A]): XmlEncoder[F, ElemValue, A] = {

    def compactEncoder: Encoder[F, C, A] =
      Encoder.fromFunction[F, C, A](compact.from)

    new XmlEncoder[F, ElemValue, A](name, (hListEncoder(children) ~ compactEncoder).encode)

  }

}