package ch.srf.xml

import scalaz.Id.Id

private[xml] trait ToTraverseDecoder[C, F[_], T[_], A] {
  def apply(c: C): TraverseDecoder[F, T, A]
}

private[xml] object ToTraverseDecoder {

  implicit def codecInstance[F[_], T[_], A]: ToTraverseDecoder[TraverseCodec[F, T, A], F, T, A] =
    new ToTraverseDecoder[TraverseCodec[F, T, A], F, T, A] {
      override def apply(c: TraverseCodec[F, T, A]): TraverseDecoder[F, T, A] =
        c.decoder
    }

  implicit def decoderInstance[F[_], T[_], A]: ToTraverseDecoder[TraverseDecoder[F, T, A], F, T, A] =
    new ToTraverseDecoder[TraverseDecoder[F, T, A], F, T, A] {
      override def apply(d: TraverseDecoder[F, T, A]): TraverseDecoder[F, T, A] =
        d
    }

  implicit def xmlDecoderInstance[F[_], X, A](implicit dfe: DecodeFromElem[F, Id, X])
  : ToTraverseDecoder[XmlDecoder[F, X, A], F, Id, A] =
    new ToTraverseDecoder[XmlDecoder[F, X, A], F, Id, A] {
      override def apply(d: XmlDecoder[F, X, A]): TraverseDecoder[F, Id,  A] =
        TraverseDecoder.fromDecoder(d)
    }

  implicit def xmlCodecInstance[F[_], X, A](implicit dfe: DecodeFromElem[F, Id, X])
  : ToTraverseDecoder[XmlCodec[F, X, A], F, Id, A] =
    new ToTraverseDecoder[XmlCodec[F, X, A], F, Id, A] {
      override def apply(c: XmlCodec[F, X, A]): TraverseDecoder[F, Id, A] =
        TraverseDecoder.fromDecoder(c.decoder)
    }

}
