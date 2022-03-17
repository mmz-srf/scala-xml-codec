package ch.srf.xml

import cats.data.NonEmptyList
import cats.Monad
import cats.syntax.all._
import ch.srf.xml.Result.Monadic
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.cats.implicits._
import org.scalacheck.{Arbitrary, Gen}

object Gens {

  implicit def nonEmptyListArb[A : Arbitrary]: Arbitrary[NonEmptyList[A]] =
    Arbitrary(
      Gen
        .nonEmptyListOf(arbitrary[A])
        .map(list => NonEmptyList.fromList(list))
        .collect { case Some(list) => list }
    )

  implicit def pathArb: Arbitrary[Path] =
    Arbitrary(
      arbitrary[NonEmptyList[(String, Option[Int])]]
        .map(Path.apply)
    )

  def resultGen[F[_]:Monad, A : Arbitrary]: Gen[Result[F, A]] =
    arbitrary[Result.Errors Either A].map(r => Result(r.pure[F]))

  implicit def resultArb[F[_]:Monad, A](implicit ev: Arbitrary[A]): Arbitrary[Result[F, A]] =
    Arbitrary(resultGen)

  def monadicResultGen[F[_]:Monad, A : Arbitrary]: Gen[Monadic[F, A]] =
    arbitrary[Result.Errors Either A].map(r => Monadic(r.pure[F]))

  implicit def monadicResultArb[F[_]:Monad, A](implicit ev: Arbitrary[A]): Arbitrary[Monadic[F, A]] =
    Arbitrary(monadicResultGen)


}
