package net.devkat.xml

import net.devkat.xml.util.WrapGen
import scalaz.Isomorphism.<~~>
import scalaz.syntax.all._
import scalaz.syntax.std.option._
import scalaz.{Compose, Contravariant, EitherT, Kleisli, Monad, \/, ~~>}

import scala.annotation.implicitNotFound

@implicitNotFound("No decoder found from ${X} to ${A}")
final class Decoder[F[_], X, A] private (val decoder: Kleisli[EitherT[F, String, ?], X, A])
  extends AnyVal {

  def decode(x: X): F[String \/ A] = decoder.run(x).run

  //TODO just an alias for compose. Really needed?
  def ~[B](other: Decoder[F, A, B])
          (implicit monadEv: Monad[F]): Decoder[F, X, B] = this >>> other
}

trait DecoderLow2 {
  implicit def generic[F[_]:Monad, X, A](implicit gen: WrapGen[A, X]): Decoder[F, X, A] =
    Decoder.fromFunction(gen.from)
}

trait DecoderLow extends DecoderLow2 {

  implicit def fromCodec[F[_], X, A](implicit codec: Codec[F, X, A]): Decoder[F, X, A] =
    codec.decoder

}

object Decoder extends DecoderLow {
  def apply[F[_]:Monad, X, A](f: X => F[String \/ A]): Decoder[F, X, A] =
    new Decoder[F, X, A](Kleisli[EitherT[F, String, ?], X, A](f andThen(EitherT(_))))

  def fromDisjunction[F[_]:Monad, X, A](f: X => String \/ A): Decoder[F, X, A] =
    apply(a => f(a).point[F])

  def fromFunction[F[_]:Monad, X, A](f: X => A): Decoder[F, X, A] =
    apply(f andThen(_.pure[String \/ ?].pure[F]))

  def fromTryCatchNonFatal[F[_]:Monad, X, A](f: X => A): Decoder[F, X, A] =
    apply(a => \/.fromTryCatchNonFatal(f(a)).leftMap(_.getMessage).point[F])

  def ensure[F[_]:Monad, A](e: Ensure[F, A]): Decoder[F, A, A] =
    apply(a => e(a).map(_.<\/(a)))

  def id[F[_]: Monad, E, I]: Decoder[F, I, I] =
    fromFunction(identity)

  def decoder2ToKleisli[F[_]]: Decoder[F, ?, ?] ~~> Kleisli[EitherT[F, String, ?], ?, ?] =
    new (Decoder[F, ?, ?] ~~> Kleisli[EitherT[F, String, ?], ?, ?]) {
      def apply[A, B](x: Decoder[F, A, B]) = x.decoder
    }
  def kleisliToDecoder[F[_]]: Kleisli[EitherT[F, String, ?], ?, ?] ~~> Decoder[F, ?, ?] =
    new (Kleisli[EitherT[F, String, ?], ?, ?] ~~> Decoder[F, ?, ?]) {
      def apply[A, B](x: Kleisli[EitherT[F, String, ?], A, B]) = new Decoder(x)
    }

  def decoder2KleisliIsoBifunctor[F[_]]: Decoder[F, ?, ?] <~~> Kleisli[EitherT[F, String, ?], ?, ?] =
    new (Decoder[F, ?, ?] <~~> Kleisli[EitherT[F, String, ?], ?, ?]) {
      lazy val from = kleisliToDecoder[F]
      lazy val to = decoder2ToKleisli[F]
    }

  //TODO we could provide all instances of Kleisli this way

  implicit def decoder2ComposeInstance[F[_] : Monad]: Compose[Decoder[F, ?, ?]] =
    Compose.fromIso(decoder2KleisliIsoBifunctor[F])

  implicit def decoder2Monad[F[_] : Monad, A]: Monad[Decoder[F, A, ?]] =
    Monad.fromIso(decoder2KleisliIsoBifunctor[F].unlift1)

  implicit def decoder2Contravariant[F[_], A]: Contravariant[Decoder[F, ?, A]] =
    Contravariant.fromIso[Decoder[F, ?, A], Kleisli[EitherT[F, String, ?], ?, A]](
      decoder2KleisliIsoBifunctor[F].unlift2[A]
    )
}
