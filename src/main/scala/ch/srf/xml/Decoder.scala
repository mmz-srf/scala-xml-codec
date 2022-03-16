package ch.srf.xml

import cats.arrow.Compose
import cats.data.EitherT
import cats.data.Kleisli
import cats.syntax.all._
import cats.{Monad, Contravariant}
import ch.srf.xml.util.WrapGen
import scala.annotation.implicitNotFound

@implicitNotFound("No decoder found from ${X} to ${A}")
final class Decoder[F[_], X, A] private (val decoder: Kleisli[EitherT[F, String, *], X, A])
  extends AnyVal {
  def decode(x: X): F[String Either A] = decoder.run(x).value

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
  def apply[F[_], X, A](f: X => F[String Either A]): Decoder[F, X, A] =
    new Decoder[F, X, A](Kleisli[EitherT[F, String, *], X, A](f andThen(EitherT(_))))

  def fromEither[F[_]:Monad, X, A](f: X => String Either A): Decoder[F, X, A] =
    apply(a => f(a).pure[F])

  def fromFunction[F[_]:Monad, X, A](f: X => A): Decoder[F, X, A] =
    apply(f andThen(_.pure[String Either *].pure[F]))

  def fromTryCatchNonFatal[F[_]:Monad, X, A](f: X => A): Decoder[F, X, A] =
    apply(a => Either.catchNonFatal(f(a)).leftMap(_.getMessage).pure[F])

  def ensure[F[_]:Monad, A](e: Ensure[F, A]): Decoder[F, A, A] =
    apply(a => e(a).map(_.toLeft(a)))

  def id[F[_]: Monad, E, I]: Decoder[F, I, I] =
    fromFunction(identity)

  def decoder2ToKleisli[F[_], X, A](dec: Decoder[F, X, A]): Kleisli[EitherT[F, String, *], X, A] =
    dec.decoder

  def kleisliToDecoder[F[_], X, A](x: Kleisli[EitherT[F, String, *], X, A]): Decoder[F, X, A] =
    new Decoder(x)

  implicit def decoder2ComposeInstance[F[_] : Monad]: Compose[Decoder[F, *, *]] = new Compose[Decoder[F, *, *]] {
    override def compose[A, B, C](f: Decoder[F,B,C], g: Decoder[F,A,B]): Decoder[F,A,C] =
      kleisliToDecoder(decoder2ToKleisli(f).compose(decoder2ToKleisli(g)))
  }

  implicit def decoder2Monad[F[_] : Monad, A]: Monad[Decoder[F, A, *]] = new Monad[Decoder[F, A, *]] {
    override def flatMap[B, C](fa: Decoder[F,A,B])(f: B => Decoder[F,A,C]): Decoder[F,A,C] =
      kleisliToDecoder(decoder2ToKleisli(fa).flatMap(f.andThen(decoder2ToKleisli)))

    override def tailRecM[B, C](a: B)(f: B => Decoder[F,A,Either[B,C]]): Decoder[F,A,C] =
      kleisliToDecoder(Monad[Kleisli[EitherT[F, String, *], A, *]].tailRecM(a)(f.andThen(decoder2ToKleisli)))

    override def pure[B](x: B): Decoder[F,A,B] =
      kleisliToDecoder(x.pure[Kleisli[EitherT[F, String, *], A, *]])

  }

  implicit def decoder2Contravariant[F[_], A]: Contravariant[Decoder[F, *, A]] = new Contravariant[Decoder[F, *, A]] {
    override def contramap[B, C](fa: Decoder[F,B,A])(f: C => B): Decoder[F,C,A] =
      kleisliToDecoder(decoder2ToKleisli(fa).local(f))
  }

}
