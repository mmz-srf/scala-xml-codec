package ch.srf.xml.util

import cats.data.NonEmptyList
import cats.Monad
import cats.syntax.all._

private[xml] trait Flatten[A, B] {
  def to(a: A): B
  def from(b: B): A
}

private[xml] object Flatten {

  def instance[F[_]:Monad, A]: Flatten[F[F[A]], F[A]] =
    new Flatten[F[F[A]], F[A]] {

      override def to(a: F[F[A]]): F[A] =
        a.flatMap(identity)

      override def from(b: F[A]): F[F[A]] =
        b.pure[F]
    }

  implicit def option[A]: Flatten[Option[Option[A]], Option[A]] =
    instance[Option, A]

  implicit def list[A]: Flatten[List[List[A]], List[A]] =
    instance[List, A]

  implicit def nonEmptyList[A]: Flatten[NonEmptyList[NonEmptyList[A]], NonEmptyList[A]] =
    instance[NonEmptyList, A]

}
