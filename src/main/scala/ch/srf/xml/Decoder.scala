package ch.srf.xml

import ch.srf.xml.util.WrapGen
import scalaz.syntax.all._
import scalaz.syntax.std.option._
import scalaz.{Monad, \/, Contravariant, EitherT, Kleisli, Compose, MonadTrans, Bind}

import scala.annotation.implicitNotFound

@implicitNotFound("No decoder found from ${X} to ${A}")
//TODO isomorphic to a Kleisli[F[String \/ ?], X, A] we should consider providing a newtype and deriving
//typeclass instances using either https://github.com/estatico/scala-newtype or by applying the iso to
//Kleisli.
abstract class Decoder[F[_], X, A] {
  outer =>

  def decode(x: X): F[String \/ A]

  //TODO just an alias for compose. Really needed?
  def ~[B](other: Decoder[F, A, B])
          (implicit monadEv: Monad[F]): Decoder[F, X, B] = outer >>> other
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

  implicit def contravariantInstance[F[_] : Monad, C]: Contravariant[Decoder[F, ?, C]] = new Contravariant[Decoder[F, ?, C]] {
    def contramap[A, B](r: Decoder[F, A, C])(f: B => A): Decoder[F, B, C] = new Decoder[F, B, C] {
      override def decode(x: B): F[String \/ C] = r.decode(f(x))
    }
  }

  implicit def monadInstance[F[_] : Monad, X]: Monad[Decoder[F, X, ?]] = new Monad[Decoder[F, X, ?]] {
    override def point[A](a: => A): Decoder[F, X, A] = new Decoder[F, X, A] {
      override def decode(x: X): F[String \/ A] = a.pure[String \/ ?].pure[F]
    }

    override def bind[A, B](fa: Decoder[F, X, A])(f: A => Decoder[F, X, B]): Decoder[F, X, B] = new Decoder[F, X, B] {
      def decode(x: X): F[String \/ B] =
        EitherT
          .apply(fa.decode(x))
          .flatMapF(f andThen (_.decode(x)))
          .run
    }
  }
  //TODO We can also implement MonadTrans..

  implicit def composeInstance[F[_] : Monad]: Compose[Decoder[F, ?, ?]] = new Compose[Decoder[F, ?, ?]] {
    def compose[A, B, C](f: Decoder[F, B, C], g: Decoder[F, A, B]): Decoder[F, A, C] = new Decoder[F, A, C] {
      def decode(a: A): F[String \/ C] =
        g.decode(a).flatMap(_.traverseM(f.decode))
    }
  }

}
