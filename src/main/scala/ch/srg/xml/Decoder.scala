package ch.srg.xml

import ch.srg.xml.util.WrapGen
import scalaz.syntax.all._
import scalaz.syntax.std.option._
import scalaz.{Monad, \/}

import scala.annotation.implicitNotFound

@implicitNotFound("No decoder found from ${X} to ${A}")
abstract class Decoder[F[_]:Monad, X, A] {
  outer =>

  def decode(x: X): F[String \/ A]

  def ~[B](other: Decoder[F, A, B]): Decoder[F, X, B] =
    new Decoder[F, X, B] {
      override def decode(x: X): F[String \/ B] =
        outer.decode(x).flatMap(_.traverseM(other.decode))
    }

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
    new Decoder[F, X, A] {
      override def decode(x: X): F[String \/ A] =
        f(x)
    }

  def fromDisjunction[F[_]:Monad, X, A](f: X => String \/ A): Decoder[F, X, A] =
    apply(a => f(a).point[F])

  def fromFunction[F[_]:Monad, X, A](f: X => A): Decoder[F, X, A] =
    new Decoder[F, X, A] {
      override def decode(x: X): F[String \/ A] =
        f(x).right[String].point[F]
    }

  def fromTryCatchNonFatal[F[_]:Monad, X, A](f: X => A): Decoder[F, X, A] =
    apply(a => \/.fromTryCatchNonFatal(f(a)).leftMap(_.getMessage).point[F])

  def ensure[F[_]:Monad, A](e: Ensure[F, A]): Decoder[F, A, A] =
    new Decoder[F, A, A] {
      override def decode(a: A): F[String \/ A] =
        e(a).map(_.<\/(a))
    }

  def id[F[_]: Monad, E, I]: Decoder[F, I, I] =
    fromFunction(identity)

}
