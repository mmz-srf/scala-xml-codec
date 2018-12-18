package ch.srf.xml

import scalaz.std.list.listInstance
import scalaz.std.option.optionInstance
import scalaz.syntax.traverse._
import scalaz.{Applicative, NonEmptyList, Traverse}

private[xml] sealed trait CardinalityDecoder[F[_], Cy[_], X, A] {

  def decode(dec: X => Result[F, A], x: Cy[X]): Result[F, Cy[A]]

}

private[xml] object CardinalityDecoder {

  def option[F[_]:Applicative, X, A]: CardinalityDecoder[F, Option, X, A] =
    new CardinalityDecoder[F, Option, X, A] {
      override def decode(dec: X => Result[F, A], x: Option[X]): Result[F, Option[A]] =
        x traverse dec
    }

  def list[F[_]:Applicative, X, A]: CardinalityDecoder[F, List, X, A] =
    new CardinalityDecoder[F, List, X, A] {
      override def decode(dec: X => Result[F, A], xs: List[X]): Result[F, List[A]] =
        decodeTraverse(dec, xs)
    }

  def nel[F[_]:Applicative, X, A]: CardinalityDecoder[F, NonEmptyList, X, A] =
    new CardinalityDecoder[F, NonEmptyList, X, A] {
      override def decode(dec: X => Result[F, A], xs: NonEmptyList[X]): Result[F, NonEmptyList[A]] =
        decodeTraverse(dec, xs)
    }

  private def decodeTraverse[
  F[_]: Applicative,
  G[_]: Traverse, X, A](dec: X => Result[F, A],
                        xs: G[X]): Result[F, G[A]] = {
    val pairs = xs.indexed.map { case (pos, x) => (x, pos + 1) }
    pairs.traverse { case (e, pos) => dec(e).updatePos(pos) }
  }

}
