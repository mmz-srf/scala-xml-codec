package net.devkat.xml

import net.devkat.xml.util.WrapGen
import scalaz.{Monad, \/}

import scala.annotation.implicitNotFound

@implicitNotFound("No codec found from ${X} to ${A}")
final class Codec[F[_], X, A](val decoder: Decoder[F, X, A], val encoder: Encoder[F, X, A]) {

  def decode(x: X): F[String \/ A] =
    decoder.decode(x)

  def encode(a: A): F[X] =
    encoder.encode(a)

  def ~[B](codec: Codec[F, A, B])
          (implicit monadEv: Monad[F]): Codec[F, X, B] =
    Codec.from(decoder ~ codec.decoder, encoder ~ codec.encoder)

}

trait CodecLow {

  implicit def generic[F[_]:Monad, X, A](implicit gen: WrapGen[A, X]): Codec[F, X, A] =
    Codec.from(Decoder.generic, Encoder.generic)

  implicit def intCodec[F[_]:Monad, X, A]: Codec[F, String, Int] =
    Codec.from(
      Decoder.fromTryCatchNonFatal(_.toInt),
      Encoder.fromFunction(_.toString)
    )

  implicit def longCodec[F[_]:Monad, X, A]: Codec[F, String, Long] =
    Codec.from(
      Decoder.fromTryCatchNonFatal(_.toLong),
      Encoder.fromFunction(_.toString)
    )

}

object Codec extends CodecLow {

  def from[F[_], X, A](dec: Decoder[F, X, A], enc: Encoder[F, X, A]): Codec[F, X, A] =
    new Codec(dec, enc)

  def fromFunctions[F[_]:Monad, X, A](dec: X => A, enc: A => X): Codec[F, X, A] =
    from(Decoder.fromFunction(dec), Encoder.fromFunction(enc))

  def ensure[F[_]:Monad, A](e: Ensure[F, A]): Codec[F, A, A] =
    from(Decoder.ensure(e), Encoder.id[F, A])
}
