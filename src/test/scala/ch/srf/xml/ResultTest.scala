package ch.srf.xml

import cats.data.NonEmptyList
import cats.Id
import cats.laws.discipline.ApplicativeTests
import cats.laws.discipline.MonadTests
import cats.syntax.all._
import cats.{Applicative, Apply, Monad}
import ch.srf.xml.Result.Monadic
import org.scalacheck.Arbitrary
import org.specs2.mutable.Specification
import org.specs2.ScalaCheck

object ResultTest extends Specification with ScalaCheck {

  private type F[T] = Option[T]

  private def run[A](result: Result[F, A]): F[NonEmptyList[String] Either A] =
    result.value.map(_.leftMap(_.map(_._2)))

  private def error(msg: String): Result[F, String] =
    Result.error[F, String](Path(("error", Option.empty[Int]).pure[NonEmptyList]), msg)

  "The Applicative instance" should {

    "correctly handle ap" in {

      def f: Result[F, String => String] =
        Result.success(_ + " changed")

      val err = error("e")
      val errorResult = Apply[Result[F, *]].ap(f)(err)
      run(errorResult) should beSome(NonEmptyList.of("e").asLeft)

      val succ = Result.success[F, String]("a")
      val successResult = Apply[Result[F, *]].ap(f)(succ)
      run(successResult) should beSome("a changed".asRight)
    }

    "correctly combine errors in apply2" in {

      val a = error("a")
      val b = error("b")

      val result = (a, b).mapN(_ + _)

      run(result) should beSome(NonEmptyList.of("a", "b").asLeft)
    }

    "correctly aggregate errors during traveral" in {

      val list = List("a", "b", "c")

      val result = list.traverse(error)

      run(result) should beSome(NonEmptyList.of("a", "b", "c").asLeft)

    }

    "obey the applicative laws" in {
      implicit val applicativeInstance: Applicative[Result[Id, *]] = Result.applicativeInstance[Id]

      implicit def arb[A : Arbitrary]: Arbitrary[Result[Id, A]] = Gens.resultArb[Id, A]

      ApplicativeTests[Result[Id, *]].applicative[String, String, String].all

    }

  }

  "The Monad instance" should {

    "obey the monad laws" in {

      implicit val monadInstance: Monad[Monadic[Id, *]] = Monadic.monadInstance[Id]
      implicit val arb: Arbitrary[Monadic[Id, Int]] = Gens.monadicResultArb[Id, Int]
      implicit val functionArb: Arbitrary[Monadic[Id, Int => Int]] = Gens.monadicResultArb[Id, Int => Int]

      MonadTests[Monadic[Id, *]].monad[Int, Int, Int].all
    }

  }

}
