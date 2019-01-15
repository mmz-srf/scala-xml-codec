package ch.srf.xml

import ch.srf.xml.util.CompactHList
import scalaz.{Applicative, NonEmptyList}
import scalaz.Id.Id

private[xml] trait ToTraverseEncoder[C, F[_], T[_], X, A] {
  def apply(c: C): TraverseEncoder[F, T, X, A]
}

private[xml] object ToTraverseEncoder {

  implicit def codecInstance[F[_], T[_], X, A]: ToTraverseEncoder[TraverseCodec[F, T, X, A], F, T, X, A] =
    new ToTraverseEncoder[TraverseCodec[F, T, X, A], F, T, X, A] {
      override def apply(c: TraverseCodec[F, T, X, A]): TraverseEncoder[F, T, X, A] =
        c.encoder
    }

  implicit def decoderInstance[F[_], T[_], X, A]: ToTraverseEncoder[TraverseEncoder[F, T, X, A], F, T, X, A] =
    new ToTraverseEncoder[TraverseEncoder[F, T, X, A], F, T, X, A] {
      override def apply(e: TraverseEncoder[F, T, X, A]): TraverseEncoder[F, T, X, A] =
        e
    }

  implicit def xmlEncoderInstance[F[_]:Applicative, X, A](implicit append: AppendToElem[X])
  : ToTraverseEncoder[XmlEncoder[F, X, A], F, Id, X, A] =
    new ToTraverseEncoder[XmlEncoder[F, X, A], F, Id, X, A] {
      override def apply(c: XmlEncoder[F, X, A]): TraverseEncoder[F, Id, X, A] =
        TraverseEncoder(c)
    }

  implicit def xmlCodecInstance[F[_]:Applicative, X, A](implicit append: AppendToElem[X])
  : ToTraverseEncoder[XmlCodec[F, X, A], F, Id, X, A] =
    new ToTraverseEncoder[XmlCodec[F, X, A], F, Id, X, A] {
      override def apply(c: XmlCodec[F, X, A]): TraverseEncoder[F, Id, X, A] =
        TraverseEncoder(c.encoder)
    }

}
