package ch.srf.xml

import scalaz.syntax.all._
import scalaz.{Applicative, EitherT, Equal, Monad, NonEmptyList, \/, ~>, Functor}

private[xml] final case class Result[F[_]:Applicative, A](value: F[Result.Errors \/ A]) {
  import Result._

  def prependPath(name: String, pos: Option[Int]): Result[F, A] =
    Result(mapErrors((path, msg) => (path.prepend(name, pos), msg)))

  def updatePos(pos: Int): Result[F, A] =
    Result(mapErrors((path, msg) => (path.updatePos(pos), msg)))

  def leftAsStrings: F[NonEmptyList[String] \/ A] =
    mapErrors((path, msg) => path.shows + ": " + msg)

  def monadic: Monadic[F, A] =
    Monadic(value)

  private def mapErrors[B](f: (Path, String) => B): F[NonEmptyList[B] \/ A] =
    Result.mapErrors(value)(f)
}

private[xml] object Result {

  private[xml] type Errors = NonEmptyList[(Path, String)]

  private def mapErrors[A, B, F[_] : Functor](value: F[Result.Errors \/ A])(f: (Path, String) => B): F[NonEmptyList[B] \/ A] =
    value.map(_.leftMap(_.map(f.tupled)))

  def success[F[_]:Applicative, A](value: A): Result[F, A] =
    value.point[Result[F, ?]]

  def error[F[_]:Applicative, A](path: Path, msg: String): Result[F, A] =
    Result((path, msg).wrapNel.left[A].point[F])

  def fromDisjunction[F[_]:Applicative, A](d: F[String \/ A], name: String): Result[F, A] =
    Result(d.map(_.leftMap(e => (Path((name, Option.empty[Int]).wrapNel), e).wrapNel)))

  implicit def applicativeInstance[F[_]:Applicative]: Applicative[Result[F, ?]] =
    new Applicative[Result[F, ?]] {
      private lazy val C = Applicative[F]
        .compose[Result.Errors \/ ?]

      override def ap[A, B](fa: => Result[F, A])(f: => Result[F, A => B]): Result[F, B] = {
        lazy val C2 = Applicative[F]
          .compose[scalaz.Validation[Result.Errors, ?]]

        Result(
          C2
            .apply2(
              f.value.map(_.validation),
              fa.value.map(_.validation)
            )(_(_))
            .map(_.disjunction)
        )
      }

      override def point[A](a: => A): Result[F, A] =
        Result(C point a)
    }

  implicit def equalInstance[F[_], A](implicit ev: Equal[F[Errors \/ A]]): Equal[Result[F, A]] =
    ev contramap (_.value)

  final case class Monadic[F[_]:Applicative, A](value: F[Errors \/ A]) {

    def applicative: Result[F, A] =
      Result(value)

  }

  object Monadic {

    implicit def monadInstance[F[_]:Monad]: Monad[Monadic[F, ?]] =
      new Monad[Monadic[F, ?]] {

        override def bind[A, B](fa: Monadic[F, A])(f: A => Monadic[F, B]): Monadic[F, B] =
          Monadic(EitherT(fa.value).flatMapF(f(_).value).run)

        override def point[A](a: => A): Monadic[F, A] =
          a.point[Result[F, ?]].monadic

      }

    implicit def equalInstance[F[_], A](implicit ev: Equal[F[Errors \/ A]]): Equal[Monadic[F, A]] =
      ev contramap (_.value)
  }

  def applicativeToMonadic[F[_]:Applicative]: Result[F, ?] ~> Monadic[F, ?] =
    new (Result[F, ?] ~> Monadic[F, ?]) {
      override def apply[A](fa: Result[F, A]): Monadic[F, A] = fa.monadic
    }

  def monadicToApplicative[F[_]:Applicative]: Monadic[F, ?] ~> Result[F, ?] =
    new (Monadic[F, ?] ~> Result[F, ?]) {
      override def apply[A](fa: Monadic[F, A]): Result[F, A] = fa.applicative
    }

}
