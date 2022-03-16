package ch.srf.xml

import cats.data.EitherT
import cats.data.Nested
import cats.data.NonEmptyList
import cats.syntax.all._
import cats.{Applicative, Eq, Monad, ~>, Functor}

private[xml] final case class Result[F[_]:Applicative, A](value: F[Result.Errors Either A]) {
  import Result._

  def prependPath(name: String, pos: Option[Int]): Result[F, A] =
    Result(mapErrors((path, msg) => (path.prepend(name, pos), msg)))

  def updatePos(pos: Int): Result[F, A] =
    Result(mapErrors((path, msg) => (path.updatePos(pos), msg)))

  def leftAsStrings: F[NonEmptyList[String] Either A] =
    mapErrors((path, msg) => path.show + ": " + msg)

  def monadic: Monadic[F, A] =
    Monadic(value)

  private def mapErrors[B](f: (Path, String) => B): F[NonEmptyList[B] Either A] =
    Result.mapErrors(value)(f)
}

private[xml] object Result {

  private[xml] type Errors = NonEmptyList[(Path, String)]

  private def mapErrors[A, B, F[_] : Functor](value: F[Result.Errors Either A])(f: (Path, String) => B): F[NonEmptyList[B] Either A] =
    value.map(_.leftMap(_.map(f.tupled)))

  def success[F[_]:Applicative, A](value: A): Result[F, A] =
    value.pure[Result[F, *]]

  def error[F[_]:Applicative, A](path: Path, msg: String): Result[F, A] =
    Result((path, msg).pure[NonEmptyList].asLeft.pure[F])

  def fromEither[F[_]:Applicative, A](d: F[String Either A], name: String): Result[F, A] =
    Result(d.map(_.leftMap(e => (Path((name, Option.empty[Int]).pure[NonEmptyList]), e).pure[NonEmptyList])))

  implicit def applicativeInstance[F[_]:Applicative]: Applicative[Result[F, *]] =
    new Applicative[Result[F, *]] {
      override def ap[A, B](f: Result[F,A => B])(fa: Result[F,A]): Result[F,B] =
        Result(
          Nested
            .apply(f.value.map(_.toValidated))
            .ap(Nested(fa.value.map(_.toValidated)))
            .value
            .map(_.toEither)
        )

      override def pure[A](x: A): Result[F,A] =
        Result(x.asRight.pure[F])
    }

  implicit def eqInstance[F[_], A](implicit ev: Eq[F[Errors Either A]]): Eq[Result[F, A]] =
    ev contramap (_.value)

  final case class Monadic[F[_]:Applicative, A](value: F[Errors Either A]) {

    def applicative: Result[F, A] =
      Result(value)

  }

  object Monadic {

    implicit def monadInstance[F[_]:Monad]: Monad[Monadic[F, *]] =
      new Monad[Monadic[F, *]] {
        override def flatMap[A, B](fa: Monadic[F, A])(f: A => Monadic[F, B]): Monadic[F, B] =
          Monadic(EitherT(fa.value).flatMapF(f(_).value).value)

        override def pure[A](a: A): Monadic[F, A] =
          a.pure[Result[F, *]].monadic

        override def tailRecM[A, B](a: A)(f: A => Monadic[F,Either[A,B]]): Monadic[F,B] =
          Monadic(
            EitherT
              .catsDataMonadErrorForEitherT[F, Errors]
              .tailRecM(a)(a => EitherT[F, Errors, Either[A, B]](f(a).value))
              .value
          )
      }

    implicit def eqInstance[F[_], A](implicit ev: Eq[F[Errors Either A]]): Eq[Monadic[F, A]] =
      ev contramap (_.value)
  }

  def applicativeToMonadic[F[_]]: Result[F, *] ~> Monadic[F, *] =
    new (Result[F, *] ~> Monadic[F, *]) {
      override def apply[A](fa: Result[F, A]): Monadic[F, A] = fa.monadic
    }

  def monadicToApplicative[F[_]]: Monadic[F, *] ~> Result[F, *] =
    new (Monadic[F, *] ~> Result[F, *]) {
      override def apply[A](fa: Monadic[F, A]): Result[F, A] = fa.applicative
    }

}
