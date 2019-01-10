package ch.srf.xml

private[xml] trait ToXmlDecoder[C[_[_], _, _]] {
  def apply[F[_], X, A](c: C[F, X, A]): XmlDecoder[F, X, A]
}

private[xml] object ToXmlDecoder {

  implicit def codecInstance: ToXmlDecoder[XmlCodec] =
    new ToXmlDecoder[XmlCodec] {
      override def apply[F[_], X, A](c: XmlCodec[F, X, A]): XmlDecoder[F, X, A] =
        c.decoder
    }

  implicit def decoderInstance: ToXmlDecoder[XmlDecoder] =
    new ToXmlDecoder[XmlDecoder] {
      override def apply[F[_], X, A](d: XmlDecoder[F, X, A]): XmlDecoder[F, X, A] =
        d
    }

}
