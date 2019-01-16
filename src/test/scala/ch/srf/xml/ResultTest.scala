package ch.srf.xml

import ch.srf.xml.Result.{Errors, Monadic}
import org.scalacheck.Arbitrary
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification
import scalaz.Id.Id
import scalaz.scalacheck.ScalazProperties
import scalaz.std.anyVal.intInstance
import scalaz.std.list.listInstance
import scalaz.std.option.optionInstance
import scalaz.std.string.stringInstance
import scalaz.std.tuple._
import scalaz.syntax.nel._
import scalaz.syntax.traverse._
import scalaz.{-\/, Applicative, Apply, Equal, Monad, NonEmptyList, \/, \/-}

object ResultTest extends Specification with ScalaCheck {

  private type F[T] = Option[T]

  private def run[A](result: Result[F, A]): F[NonEmptyList[String] \/ A] =
    result.value.map(_.leftMap(_.map(_._2)))

  private def error(msg: String): Result[F, String] =
    Result.error[F, String](msg)

  "The Applicative instance" should {

    "correctly handle ap" in {

      def f: Result[F, String => String] =
        Result.success(_ + " changed")

      val err = error("e")
      val errorResult = Apply[Result[F, ?]].ap(err)(f)
      run(errorResult) should beSome(-\/(NonEmptyList("e")))

      val succ = Result.success[F, String]("a")
      val successResult = Apply[Result[F, ?]].ap(succ)(f)
      run(successResult) should beSome(\/-("a changed"))

    }

    "correctly combine errors in apply2" in {

      val a = error("a")
      val b = error("b")

      val result = Apply[Result[F, ?]].apply2(a, b) { _ + _ }

      run(result) should beSome(-\/(NonEmptyList("a", "b")))

    }

    "correctly aggregate errors during traveral" in {

      val list = List("a", "b", "c")

      val result = list.traverse(error)

      run(result) should beSome(-\/(NonEmptyList("a", "b", "c")))

    }

    "obey the applicative laws" in {

      implicit val applicativeInstance: Applicative[Result[Id, ?]] = Result.applicativeInstance[Id]
      implicit val arb: Arbitrary[Result[Id, Int]] = Gens.resultArb[Id, Int]
      implicit val functionArb: Arbitrary[Result[Id, Int => Int]] = Gens.resultArb[Id, Int => Int]
      implicit val eeq: scalaz.Equal[Id[Errors \/ Int]] = Equal.equalA
      implicit val eq: Equal[Result[Id, Int]] = Result.equalInstance[Id, Int]

      ScalazProperties.applicative.laws[Result[Id, ?]]

    }

  }

  "The Monad instance" should {

    "obey the monad laws" in {

      implicit val monadInstance: Monad[Monadic[Id, ?]] = Monadic.monadInstance[Id]
      implicit val arb: Arbitrary[Monadic[Id, Int]] = Gens.monadicResultArb[Id, Int]
      implicit val functionArb: Arbitrary[Monadic[Id, Int => Int]] = Gens.monadicResultArb[Id, Int => Int]
      implicit val eeq: scalaz.Equal[Id[Errors \/ Int]] = Equal.equalA
      implicit val eq: Equal[Monadic[Id, Int]] = Monadic.equalInstance[Id, Int]

      ScalazProperties.monad.laws[Monadic[Id, ?]]

    }

  }

}
