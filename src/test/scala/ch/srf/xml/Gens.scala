package ch.srf.xml

import ch.srf.xml.Result.Monadic
import ch.srf.xml.Result.Monadic
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.{Arbitrary, Gen}
import scalaz.scalacheck.ScalazArbitrary._
import scalaz.syntax.applicative._
import scalaz.{Monad, NonEmptyList, \/}

object Gens {

  implicit def pathArb: Arbitrary[Path] =
    Arbitrary(
      arbitrary[NonEmptyList[(String, Option[Int])]].map(Path.apply)
    )

  def resultGen[F[_]:Monad, A](implicit ev: Arbitrary[A]): Gen[Result[F, A]] =
    arbitrary[Result.Error \/ A].map(r => Result(r.point[F]))

  implicit def resultArb[F[_]:Monad, A](implicit ev: Arbitrary[A]): Arbitrary[Result[F, A]] =
    Arbitrary(resultGen)

  def monadicResultGen[F[_]:Monad, A](implicit ev: Arbitrary[A]): Gen[Monadic[F, A]] =
    arbitrary[Result.Error \/ A].map(r => Monadic(r.point[F]))

  implicit def monadicResultArb[F[_]:Monad, A](implicit ev: Arbitrary[A]): Arbitrary[Monadic[F, A]] =
    Arbitrary(monadicResultGen)


}
