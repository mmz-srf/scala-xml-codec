package ch.srf.xml

import ch.srf.xml.util.{CompactHList, Flatten}
import scalaz.Id.Id
import scalaz.{@@, Monad, NonEmptyList, Traverse, \/}

import scala.xml.Elem

final class XmlCodec[F[_]:Monad, D, C[_], X, A](val decoder: XmlDecoder[F, D, C, X, A],
                                                val encoder: XmlEncoder[F, D, C, X, A]) {

  def as[B](implicit codec: Codec[F, C[A], C[B]]): XmlCodec[F, D, C, X, B] =
    this ~ codec

  def ~[B](codec: Codec[F, C[A], C[B]]): XmlCodec[F, D, C, X, B] =
    new XmlCodec[F, D, C, X, B](
      decoder ~ codec.decoder,
      encoder ~ codec.encoder)

  def ensure(e: Ensure[F, C[A]]): XmlCodec[F, D, C, X, A] =
    new XmlCodec(decoder.ensure(e), encoder)

  def skip[B](implicit ev: Flatten[C[A], C[B]]): XmlCodec[F, D, C, X, B] =
    new XmlCodec(decoder.skip, encoder.skip)

  def decode(x: C[X]): F[NonEmptyList[String] \/ C[A]] =
    decoder.decode(x)

  def decodeFromParent(e: Elem)(implicit ev: GetFromElem[F, D, C, X]): F[NonEmptyList[String] \/ C[A]] =
    decoder.decodeFromParent(e)

  def encode(a: C[A]): F[C[X]] =
    encoder.encode(a)

  def when(filter: X => F[Boolean]): XmlCodec[F, D, C, X, A] =
    new XmlCodec[F, D, C, X, A](
      decoder.when(filter),
      encoder
    )

}

object XmlCodec {

  def collection[F[_]:Monad, C[_], D, X, A](codec: XmlCodec[F, D, Id, X, A], cd: CardinalityDecoder[F, C, X, A])
                                           (implicit traverseEv: Traverse[C]): XmlCodec[F, D, C, X, A] =
    new XmlCodec[F, D, C, X, A](
      XmlDecoder.collection[F, C, D, X, A](codec.decoder, cd),
      XmlEncoder.collection[F, C, D, X, A](codec.encoder)
    )

  def text[F[_]:Monad]: XmlCodec[F, Unit, Id, String @@ TextValue, String] =
    new XmlCodec(XmlDecoder.text, XmlEncoder.text)

  def nonEmptyText[F[_]:Monad]: XmlCodec[F, Unit, Id, String @@ NonEmptyTextValue, String] =
    new XmlCodec(XmlDecoder.nonEmptyText, XmlEncoder.nonEmptyText)

  def attr[F[_]:Monad](name: String): XmlCodec[F, String, Id, String @@ AttrValue, String] =
    new XmlCodec(XmlDecoder.attr(name), XmlEncoder.attr(name))

  def elem[F[_]:Monad, CS, C, A](name: String, children: CS)
                                (implicit
                                 hListDecoder: HListDecoder[F, CS, C],
                                 hListEncoder: HListEncoder[F, CS, C],
                                 compact: CompactHList[C, A]): XmlCodec[F, String, Id, Elem, A] =
    new XmlCodec(XmlDecoder.elem(name, children), XmlEncoder.elem(name, children))

}