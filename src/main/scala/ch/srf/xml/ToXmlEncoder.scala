package ch.srf.xml

private[xml] trait ToXmlEncoder[CC[_[_], _, _[_], _, _]] {
  def apply[F[_], D, C[_], X, A](c: CC[F, D, C, X, A]): XmlEncoder[F, D, C, X, A]
}

private[xml] object ToXmlEncoder {

  implicit def codecInstance: ToXmlEncoder[XmlCodec] =
    new ToXmlEncoder[XmlCodec] {
      override def apply[F[_], D, C[_], X, A](c: XmlCodec[F, D, C, X, A]): XmlEncoder[F, D, C, X, A] =
        c.encoder
    }

  implicit def encoderInstance: ToXmlEncoder[XmlEncoder] =
    new ToXmlEncoder[XmlEncoder] {
      override def apply[F[_], D, C[_], X, A](d: XmlEncoder[F, D, C, X, A]): XmlEncoder[F, D, C, X, A] =
        d
    }

}

