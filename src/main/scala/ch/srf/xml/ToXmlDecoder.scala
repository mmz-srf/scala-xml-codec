package ch.srf.xml

private[xml] trait ToXmlDecoder[C, F[_], X, A] {

  def apply(c: C): XmlDecoder[F, X, A]

}

private[xml] object ToXmlDecoder {

  implicit def codecInstance[F[_], X, A]: ToXmlDecoder[XmlCodec[F, X, A], F, X, A] =
    new ToXmlDecoder[XmlCodec[F, X, A], F, X, A] {
      override def apply(c: XmlCodec[F, X, A]): XmlDecoder[F, X, A] =
        c.decoder
    }

  implicit def decoderInstance[F[_], X, A]: ToXmlDecoder[XmlDecoder[F, X, A], F, X, A] =
    new ToXmlDecoder[XmlDecoder[F, X, A], F, X, A] {
      override def apply(d: XmlDecoder[F, X, A]): XmlDecoder[F, X, A] =
        d
    }

}
