package ch.srf.xml

private[xml] trait ToXmlDecoder[CC[_[_], _, _[_], _, _]] {
  def apply[F[_], D, C[_], X, A](c: CC[F, D, C, X, A]): XmlDecoder[F, D, C, X, A]
}

private[xml] object ToXmlDecoder {

  implicit def codecInstance: ToXmlDecoder[XmlCodec] =
    new ToXmlDecoder[XmlCodec] {
      override def apply[F[_], D, C[_], X, A](c: XmlCodec[F, D, C, X, A]): XmlDecoder[F, D, C, X, A] =
        c.decoder
    }

  implicit def decoderInstance: ToXmlDecoder[XmlDecoder] =
    new ToXmlDecoder[XmlDecoder] {
      override def apply[F[_], D, C[_], X, A](d: XmlDecoder[F, D, C, X, A]): XmlDecoder[F, D, C, X, A] =
        d
    }

}
