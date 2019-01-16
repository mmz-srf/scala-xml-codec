package ch.srf.xml

import scalaz.Id.Id

private[xml] trait ToTraverseDecoder[C, F[_], T[_], X, A] {
  def apply(c: C): TraverseDecoder[F, T, X, A]
}

private[xml] object ToTraverseDecoder {

  implicit def codecInstance[F[_], T[_], X, A]: ToTraverseDecoder[TraverseCodec[F, T, X, A], F, T, X, A] =
    new ToTraverseDecoder[TraverseCodec[F, T, X, A], F, T, X, A] {
      override def apply(c: TraverseCodec[F, T, X, A]): TraverseDecoder[F, T, X, A] =
        c.decoder
    }

  implicit def decoderInstance[F[_], T[_], X, A]: ToTraverseDecoder[TraverseDecoder[F, T, X, A], F, T, X, A] =
    new ToTraverseDecoder[TraverseDecoder[F, T, X, A], F, T, X, A] {
      override def apply(d: TraverseDecoder[F, T, X, A]): TraverseDecoder[F, T, X, A] =
        d
    }

  implicit def xmlDecoderInstance[F[_], X, A](implicit dfe: DecodeFromElem[F, Id, X])
  : ToTraverseDecoder[XmlDecoder[F, X, A], F, Id, X, A] =
    new ToTraverseDecoder[XmlDecoder[F, X, A], F, Id, X, A] {
      override def apply(d: XmlDecoder[F, X, A]): TraverseDecoder[F, Id, X, A] =
        TraverseDecoder.fromDecoder(d)
    }

  implicit def xmlCodecInstance[F[_], X, A](implicit dfe: DecodeFromElem[F, Id, X])
  : ToTraverseDecoder[XmlCodec[F, X, A], F, Id, X, A] =
    new ToTraverseDecoder[XmlCodec[F, X, A], F, Id, X, A] {
      override def apply(c: XmlCodec[F, X, A]): TraverseDecoder[F, Id, X, A] =
        TraverseDecoder.fromDecoder(c.decoder)
    }

}
