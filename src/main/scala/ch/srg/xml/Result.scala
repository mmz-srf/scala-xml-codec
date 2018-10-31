package ch.srg.xml

import scalaz.syntax.all._
import scalaz.{Applicative, EitherT, Equal, Monad, NonEmptyList, \/, ~>}

final case class Result[F[_]:Applicative, A](value: F[Error \/ A]) {
  import Result._

  def prependPath(name: String, pos: Option[Int]): Result[F, A] =
    Result(value.map(_.leftMap(_.map { case (path, msg) => (path.prepend(name, pos), msg) } )))

  def updatePos(pos: Int): Result[F, A] =
    Result(value.map(_.leftMap(_.map { case (path, msg) => (path.updatePos(pos), msg) } )))

  def leftAsStrings: F[NonEmptyList[String] \/ A] =
    value.map(_.leftMap(_.map { case (path, msg) => path.shows + ": " + msg }))

  def monadic: Monadic[F, A] =
    Monadic(value)

}

object Result {

  def success[F[_]:Applicative, A](value: A): Result[F, A] =
    value.point[Result[F, ?]]

  def error[F[_]:Applicative, A](path: Path, msg: String): Result[F, A] =
    Result((path, msg).wrapNel.left[A].point[F])

  def fromDisjunction[F[_]:Applicative, A](d: F[String \/ A], name: String): Result[F, A] =
    Result(d.map(_.leftMap(e => (Path((name, Option.empty[Int]).wrapNel), e).wrapNel)))

  implicit def applicativeInstance[F[_]:Applicative]: Applicative[Result[F, ?]] =
    new Applicative[Result[F, ?]] {

      override def ap[A, B](fa: => Result[F, A])(f: => Result[F, A => B]): Result[F, B] =
        Result((f.value |@| fa.value) {
          case (ff, aa) => (ff.validation |@| aa.validation) { case (fff, aaa) => fff(aaa) }.disjunction
        })

      override def point[A](a: => A): Result[F, A] =
        Result(a.right[Error].point[F])

    }

  implicit def equalInstance[F[_], A](implicit ev: Equal[F[Error \/ A]]): Equal[Result[F, A]] =
    Equal.equalBy[Result[F, A], F[Error \/ A]](_.value)

  final case class Monadic[F[_]:Applicative, A](value: F[Error \/ A]) {

    def applicative: Result[F, A] =
      Result(value)

  }

  object Monadic {

    implicit def monadInstance[F[_]:Monad]: Monad[Monadic[F, ?]] =
      new Monad[Monadic[F, ?]] {

        override def bind[A, B](fa: Monadic[F, A])(f: A => Monadic[F, B]): Monadic[F, B] =
          Monadic(EitherT(fa.value).flatMap(x => EitherT(f(x).value)).run)

        override def point[A](a: => A): Monadic[F, A] =
          a.point[Result[F, ?]].monadic

      }

    implicit def equalInstance[F[_], A](implicit ev: Equal[F[Error \/ A]]): Equal[Monadic[F, A]] =
      Equal.equalBy[Monadic[F, A], F[Error \/ A]](_.value)

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
