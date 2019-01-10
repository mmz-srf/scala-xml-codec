package ch.srf.xml

private[xml] trait ToXmlEncoder[C[_[_], _, _]] {
  def apply[F[_], X, A](c: C[F, X, A]): XmlEncoder[F, X, A]
}

private[xml] object ToXmlEncoder {

  implicit def codecInstance: ToXmlEncoder[XmlCodec] =
    new ToXmlEncoder[XmlCodec] {
      override def apply[F[_], X, A](c: XmlCodec[F, X, A]): XmlEncoder[F, X, A] =
        c.encoder
    }

  implicit def encoderInstance: ToXmlEncoder[XmlEncoder] =
    new ToXmlEncoder[XmlEncoder] {
      override def apply[F[_], X, A](d: XmlEncoder[F, X, A]): XmlEncoder[F, X, A] =
        d
    }

}

