package net.devkat.xml

import scalaz.{Monad, NonEmptyList, \/}

import scala.xml.Elem

final case class XmlDecoder[F[_], X, A](name: String,
                                        dec: X => Result[F, A],
                                        filter: X => Result[F, Boolean]) {

  def decode(x: X): F[NonEmptyList[String] \/ A] =
    dec(x).leftAsStrings

}

object XmlDecoder {

  private def nopFilter[F[_]:Monad, A]: A => Result[F, Boolean] =
    _ => Result.success(true)

  def collection[F[_]:Monad, C[_], X, A](dec: XmlDecoder[F, X, A])
                                        (implicit dfe: DecodeFromElem[F, C, X]): XmlDecoder[F, Elem, C[A]] =
    XmlDecoder("", dfe(dec.name, dec.dec, dec.filter, _), nopFilter)

  def elem[F[_], C, A]: XmlDecoder[F, C, A] = ???

}

