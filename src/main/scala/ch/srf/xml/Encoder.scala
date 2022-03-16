package ch.srf.xml

import cats.Functor
import cats.syntax.all._
import cats.{Contravariant, Monad}
import ch.srf.xml.Encoder.fromFunction
import ch.srf.xml.util.WrapGen
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

  final def xFunctor: Encoder.XFunctor[F, A, X] = new Encoder.XFunctor(this)
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

  sealed class XFunctor[F[_], A, X](val encoder: Encoder[F, X, A])

  object XFunctor {
    implicit def functorInstance[F[_]:Monad, X]: Functor[XFunctor[F, X, *]] = new Functor[XFunctor[F, X, *]] {
      override def map[A, B](fa: XFunctor[F, X, A])(f: A => B): XFunctor[F,X,B] = new XFunctor[F, X, B](
        new Encoder[F, B, X] {
          override def encode(a: X): F[B] = fa.encoder.encode(a).map(f)
        }
      )
    }
  }

  def apply[F[_]:Monad, X, A](enc: A => F[X]): Encoder[F, X, A] =
    new Encoder[F, X, A] {
      override def encode(a: A): F[X] =
        enc(a)
    }

  def fromFunction[F[_]:Monad, X, A](enc: A => X): Encoder[F, X, A] =
    Encoder(enc(_).pure[F])

  def id[F[_]: Monad, A]: Encoder[F, A, A] =
    fromFunction(identity)

  implicit def contravariantInstance[F[_]:Monad, X]: Contravariant[Encoder[F, X, *]] =
    new Contravariant[Encoder[F, X, *]] {
      override def contramap[A, B](e: Encoder[F, X, A])(f: B => A): Encoder[F, X, B] =
        new Encoder[F, X, B] {
          override def encode(b: B): F[X] =
            e.encode(f(b))
        }
    }
}
