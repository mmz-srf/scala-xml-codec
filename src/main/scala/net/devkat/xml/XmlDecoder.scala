package net.devkat.xml

import scalaz.syntax.all._
import scalaz.{EitherT, Monad}

import scala.xml.Elem

trait XmlDecoder[F[_], X, A] {
  outer =>

  def dec(x: X): Result[F, A]

  def as[B](implicit dec: Decoder[F, A, B], monadEv: Monad[F]): XmlDecoder[F, X, B] =
    this ~ dec

  def ~[B](d: Decoder[F, A, B])(implicit monadEv: Monad[F]): XmlDecoder[F, X, B] =
    new XmlDecoder[F, X, B] {
      override def dec(x: X): Result[F, B] =
        outer.dec(x).flatMap(a => EitherT(d.decode(a).map(_.leftMap(e => (List.empty[String], e).wrapNel))))
    }

  def ensure(e: Ensure[F, A])(implicit monadEv: Monad[F]): XmlDecoder[F, X, A] =
    this ~ Decoder.ensure(e)

}

trait FromElemDecoder[F[_], X, A] {
  def dec(e: Elem): Result[F, A]
}

object XmlDecoder {

  def attr[F[_]:Monad](name: String) =
    new XmlDecoder[F, String, String] {
      override def dec(x: String): Result[F, String] = x.point[Result[F, ?]]
    }

  def optional[F[_]:Monad, X, A](dec: XmlDecoder[F, X, A]) =
    new XmlDecoder[F, Option[String], Option[A]] {
      override def dec(x: String): Result[F, String] = x.point[Result[F, ?]]
    }

  def text[F[_]:Monad](name: String): XmlDecoder[F, String, String] =
    new XmlDecoder[F, String, String] {
      override def dec(x: String): Result[F, String] = x.point[Result[F, ?]]
    }


}