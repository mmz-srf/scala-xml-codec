package ch.srf.xml

import ch.srf.xml.util.{CompactHList, Flatten}
import scalaz.Id.Id
import scalaz.{@@, Monad, NonEmptyList, Traverse, \/}

import scala.xml.Elem

final class XmlCodec[F[_]:Monad, D, X, A](val decoder: XmlDecoder[F, D, X, A],
                                          val encoder: XmlEncoder[F, D, X, A]) {

  def as[B](implicit codec: Codec[F, A, B]): XmlCodec[F, D, X, B] =
    this ~ codec

  def ~[B](codec: Codec[F, A, B]): XmlCodec[F, D, X, B] =
    new XmlCodec[F, D, X, B](
      decoder ~ codec.decoder,
      encoder ~ codec.encoder)

  def ensure(e: Ensure[F, A]): XmlCodec[F, D, X, A] =
    new XmlCodec(decoder.ensure(e), encoder)

  def skip[B](implicit ev: Flatten[A, B]): XmlCodec[F, D, X, B] =
    new XmlCodec(decoder.skip, encoder.skip)

  def decode(x: X): F[NonEmptyList[String] \/ A] =
    decoder.decode(x)

  def decodeFromParent(e: Elem): F[NonEmptyList[String] \/ A] =
    decoder.decodeFromParent(e)

  def encode(a: A): F[X] =
    encoder.encode(a)

}

object XmlCodec {

  def collection[F[_]:Monad, C[_], D, X, A](codec: XmlCodec[F, D, X, A], cd: CardinalityDecoder[F, C, X, A])
                                           (implicit
                                            traverseEv: Traverse[C],
                                            getFromElem: GetFromElem[F, D, C, X]): XmlCodec[F, D, C[X], C[A]] =
    new XmlCodec[F, D, C[X], C[A]](
      XmlDecoder.collection[F, C, D, X, A](codec.decoder, cd),
      XmlEncoder.collection[F, C, D, X, A](codec.encoder)
    )

  def text[F[_]:Monad]: XmlCodec[F, Unit, String @@ TextValue, String] =
    new XmlCodec(XmlDecoder.text, XmlEncoder.text)

  def nonEmptyText[F[_]:Monad]: XmlCodec[F, Unit, String @@ NonEmptyTextValue, String] =
    new XmlCodec(XmlDecoder.nonEmptyText, XmlEncoder.nonEmptyText)

  def attr[F[_]:Monad](name: String): XmlCodec[F, String, String @@ AttrValue, String] =
    new XmlCodec(XmlDecoder.attr(name), XmlEncoder.attr(name))

  def elem[F[_]:Monad, CS, C, A](name: String, children: CS)
                                (implicit
                                 hListDecoder: HListDecoder[F, CS, C],
                                 hListEncoder: HListEncoder[F, CS, C],
                                 compact: CompactHList[C, A]): XmlCodec[F, String, Elem, A] =
    new XmlCodec(XmlDecoder.elem(name, children), XmlEncoder.elem(name, children))

}