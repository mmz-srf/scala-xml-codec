package ch.srg.xml

private[xml] trait ToXmlEncoder[C[_[_], _, _, _]] {
  def apply[F[_], D, X, A](c: C[F, D, X, A]): XmlEncoder[F, D, X, A]
}

private[xml] object ToXmlEncoder {

  implicit def codecInstance: ToXmlEncoder[XmlCodec] =
    new ToXmlEncoder[XmlCodec] {
      override def apply[F[_], D, X, A](c: XmlCodec[F, D, X, A]): XmlEncoder[F, D, X, A] =
        c.encoder
    }

  implicit def encoderInstance: ToXmlEncoder[XmlEncoder] =
    new ToXmlEncoder[XmlEncoder] {
      override def apply[F[_], D, X, A](d: XmlEncoder[F, D, X, A]): XmlEncoder[F, D, X, A] =
        d
    }

}

