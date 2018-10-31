package ch.srg.xml

import scalaz.std.list.listInstance
import scalaz.std.option.optionInstance
import scalaz.syntax.traverse._
import scalaz.{Monad, NonEmptyList}

trait CardinalityDecoder[F[_], Cy[_], X, A] {

  def decode(name: String, dec: X => Result[F, A], x: Cy[X]): Result[F, Cy[A]]

}

object CardinalityDecoder {

  def option[F[_]:Monad, I, X, A]: CardinalityDecoder[F, Option, X, A] =
    new CardinalityDecoder[F, Option, X, A] {
      override def decode(name: String, dec: X => Result[F, A], x: Option[X]): Result[F, Option[A]] =
        x.traverse(dec)

    }

  def list[F[_]:Monad, I, X, A]: CardinalityDecoder[F, List, X, A] =
    new CardinalityDecoder[F, List, X, A] {
      override def decode(name: String, dec: X => Result[F, A], xs: List[X]): Result[F, List[A]] = {
        val pairs = xs.zipWithIndex.map { case (x, pos) => (x, pos + 1) }
        pairs.traverse { case (e, pos) => dec(e).updatePos(pos) }
      }

    }

  def nel[F[_]:Monad, I, X, A]: CardinalityDecoder[F, NonEmptyList, X, A] =
    new CardinalityDecoder[F, NonEmptyList, X, A] {
      override def decode(name: String, dec: X => Result[F, A], xs: NonEmptyList[X]): Result[F, NonEmptyList[A]] = {
        val pairs = xs.zipWithIndex.map { case (x, pos) => (x, pos + 1) }
        pairs.traverse { case (e, pos) => dec(e).updatePos(pos) }
      }
    }

}