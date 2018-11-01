package ch.srg.xml.util

import scalaz.{Monad, NonEmptyList}
import scalaz.std.list.listInstance
import scalaz.std.option.optionInstance
import scalaz.syntax.monad._

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
        b.point[F]
    }

  implicit def option[A]: Flatten[Option[Option[A]], Option[A]] =
    instance[Option, A](optionInstance)

  implicit def list[A]: Flatten[List[List[A]], List[A]] =
    instance[List, A](listInstance)

  implicit def nonEmptyList[A]: Flatten[NonEmptyList[NonEmptyList[A]], NonEmptyList[A]] =
    instance[NonEmptyList, A](NonEmptyList.nonEmptyList)

}
