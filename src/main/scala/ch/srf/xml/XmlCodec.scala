package ch.srf.xml

import ch.srf.xml.util.CompactHList
import scalaz.{Functor, Monad, NonEmptyList, Traverse, \/}

import scala.xml.Elem

final case class XmlCodec[F[_], X, A](decoder: XmlDecoder[F, X, A],
                                      encoder: XmlEncoder[F, X, A]) {

  def as[B](implicit codec: Codec[F, A, B],
            monadEv: Monad[F]): XmlCodec[F, X, B] =
    this ~ codec

  def ~[B](codec: Codec[F, A, B])(implicit monadEv: Monad[F]): XmlCodec[F, X, B] =
    new XmlCodec[F, X, B](
      decoder ~ codec.decoder,
      encoder ~ codec.encoder)

  def ensure(e: Ensure[F, A])
            (implicit monadEv: Monad[F]): XmlCodec[F, X, A] =
    new XmlCodec(decoder.ensure(e), encoder)

  def decode(e: Elem)
            (implicit ev: ElemValue =:= X): F[NonEmptyList[String] \/ A] =
    decoder.decode(e)

  def decodeFromParent(e: Elem)
                      (implicit ev: ElemValue =:= X): F[NonEmptyList[String] \/ A] =
    decoder.decodeFromParent(e)

  def encode(a: A)(implicit
                   ev: X =:= ElemValue,
                   functorEv: Functor[F]): F[Elem] =
    encoder.encode(a)

}

object XmlCodec {

  def collection[F[_]:Monad, C[_]:Traverse, X, A](codec: XmlCodec[F, X, A])
                                                 (implicit
                                                  dfe: DecodeFromElem[F, C, X],
                                                  append: AppendToElem[X]): TraverseCodec[F, C, X, A] =
    TraverseCodec(
      XmlDecoder.collection[F, C, X, A](codec.decoder),
      XmlEncoder.collection[F, C, X, A](codec.encoder)
    )

  def text[F[_]:Monad]: XmlCodec[F, TextValue, String] =
    XmlCodec(XmlDecoder.text, XmlEncoder.text)

  def nonEmptyText[F[_]:Monad]: XmlCodec[F, NonEmptyTextValue, String] =
    XmlCodec(XmlDecoder.nonEmptyText, XmlEncoder.nonEmptyText)

  def attr[F[_]:Monad](name: String): XmlCodec[F, AttrValue, String] =
    XmlCodec(XmlDecoder.attr(name), XmlEncoder.attr(name))

  def elem[F[_]:Monad, CS, C, A](name: String, children: CS)
                                (implicit
                                 hListDecoder: HListDecoder[F, CS, C],
                                 hListEncoder: HListEncoder[F, CS, C],
                                 compact: CompactHList[C, A]): XmlCodec[F, ElemValue, A] =
    XmlCodec(
      XmlDecoder.elem(name, children),
      XmlEncoder.elem(name, children)
    )

}