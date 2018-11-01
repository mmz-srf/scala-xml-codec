package ch.srg.xml

private[xml] trait ToXmlDecoder[C[_[_], _, _, _]] {
  def apply[F[_], D, X, A](c: C[F, D, X, A]): XmlDecoder[F, D, X, A]
}

private[xml] object ToXmlDecoder {

  implicit def codecInstance: ToXmlDecoder[XmlCodec] =
    new ToXmlDecoder[XmlCodec] {
      override def apply[F[_], D, X, A](c: XmlCodec[F, D, X, A]): XmlDecoder[F, D, X, A] =
        c.decoder
    }

  implicit def decoderInstance: ToXmlDecoder[XmlDecoder] =
    new ToXmlDecoder[XmlDecoder] {
      override def apply[F[_], D, X, A](d: XmlDecoder[F, D, X, A]): XmlDecoder[F, D, X, A] =
        d
    }

}
