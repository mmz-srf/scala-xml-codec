package ch.srf.xml

import ch.srf.xml.util.Flatten
import scalaz.Leibniz.===
import scalaz.{Applicative, Monad}

final case class TraverseCodec[F[_], T[_], X, A](decoder: TraverseDecoder[F, T, X, A],
                                                 encoder: TraverseEncoder[F, T, A]) {

  def as[B](implicit c: Codec[F, T[A], T[B]], monadEv: Monad[F]): TraverseCodec[F, T, X, B] =
    this ~ c

  def ~[B](codec: Codec[F, T[A], T[B]])(implicit monadEv: Monad[F]): TraverseCodec[F, T, X, B] =
    copy(
      decoder = decoder ~ codec.decoder,
      encoder = encoder ~ codec.encoder
    )

  def ensure(e: Ensure[F, T[A]])(implicit monadEv: Monad[F]): TraverseCodec[F, T, X, A] =
    copy(decoder = decoder.ensure(e))

  def skip[B](implicit
              flattenEv: Flatten[T[A], T[B]],
              applicativeEv: Applicative[F]): TraverseCodec[F, T, X, B] =
    copy(decoder.skip, encoder.skip)

}
