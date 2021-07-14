package ch.srf.xml

import scalaz.syntax.applicative._
import scalaz.syntax.equal._
import scalaz.syntax.std.boolean._
import scalaz.{Applicative, Equal}

abstract class EnsureOps {

  def check[F[_]:Applicative, A](f: A => Boolean, msg: A => String): Ensure[F, A] =
    value => (!f(value)).option(msg(value)).point[F]

  def nonEmpty[F[_]:Applicative]: Ensure[F, String] =
    check(!_.isEmpty, _ => "String must not be empty")

  @SuppressWarnings(Array("org.wartremover.warts.ToString"))
  def mustEqual[F[_]:Applicative, A](a: A)(implicit ev: Equal[A]): Ensure[F, A] =
    check(_ === a, a => s"Value must equal '${a.toString}'")

}

object EnsureOps extends EnsureOps
