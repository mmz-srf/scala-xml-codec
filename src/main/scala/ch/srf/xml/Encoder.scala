package ch.srf.xml

import ch.srf.xml.Encoder.fromFunction
import ch.srf.xml.util.WrapGen
import scalaz.syntax.all._
import scalaz.{Contravariant, Monad}

import scala.annotation.implicitNotFound

@implicitNotFound("No encoder found from ${A} to ${X}")
abstract class Encoder[F[_]:Monad, X, A] {
  outer =>

  def encode(a: A): F[X]

  def ~[B](other: Encoder[F, A, B]): Encoder[F, X, B] =
    new Encoder[F, X, B] {
      override def encode(b: B): F[X] =
        other.encode(b).flatMap(outer.encode)
    }

}

trait EncoderLow2 {

  implicit def generic[F[_] : Monad, X, A](implicit gen: WrapGen[A, X]): Encoder[F, X, A] =
    fromFunction(gen.to)

}

trait EncoderLow extends EncoderLow2 {

  implicit def fromCodec[F[_], X, A](implicit codec: Codec[F, X, A]): Encoder[F, X, A] =
    codec.encoder

}

object Encoder extends EncoderLow {

  def apply[F[_]:Monad, X, A](enc: A => F[X]): Encoder[F, X, A] =
    new Encoder[F, X, A] {
      override def encode(a: A): F[X] =
        enc(a)
    }

  def fromFunction[F[_]:Monad, X, A](enc: A => X): Encoder[F, X, A] =
    Encoder(enc(_).point[F])

  def id[F[_]: Monad, A]: Encoder[F, A, A] =
    fromFunction(identity)

  implicit def contravariantInstance[F[_]:Monad, X]: Contravariant[Encoder[F, X, ?]] =
    new Contravariant[Encoder[F, X, ?]] {
      override def contramap[A, B](e: Encoder[F, X, A])(f: B => A): Encoder[F, X, B] =
        new Encoder[F, X, B] {
          override def encode(b: B): F[X] =
            e.encode(f(b))
        }
    }

}
