package ch.srf.xml

import cats.syntax.all._
import cats.{Applicative, Eq}

abstract class EnsureOps {

  def check[F[_]:Applicative, A](f: A => Boolean, msg: A => String): Ensure[F, A] =
    value => (if(f(value)) None else msg(value).some).pure[F]

  def nonEmpty[F[_]:Applicative]: Ensure[F, String] =
    check(!_.isEmpty, _ => "String must not be empty")

  @SuppressWarnings(Array("org.wartremover.warts.ToString"))
  def mustEqual[F[_]:Applicative, A](a: A)(implicit ev: Eq[A]): Ensure[F, A] =
    check(_ === a, a => s"Value must equal '${a.toString}'")

}

object EnsureOps extends EnsureOps
