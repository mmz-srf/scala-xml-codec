package ch.srf.xml

import ch.srf.xml.util.Flatten
import scalaz.{Applicative, Monad}
import scalaz.syntax.all._

final case class TraverseDecoder[F[_], T[_], A](decode: ElemValue => Result[F, T[A]]) {

  def as[B](implicit dec: Decoder[F, T[A], T[B]], monadEv: Monad[F]): TraverseDecoder[F, T, B] =
    this ~ dec

  def ~[B](d: Decoder[F, T[A], T[B]])(implicit monadEv: Monad[F]): TraverseDecoder[F, T, B] =
    copy(
      decode = x => decode(x).monadic.flatMap(a => Result.fromDisjunction(d.decode(a)).monadic).applicative
    )

  def ensure(e: Ensure[F, T[A]])(implicit monadEv: Monad[F]): TraverseDecoder[F, T, A] =
    this ~ Decoder.ensure(e)

  def skip[B](implicit
              flattenEv: Flatten[T[A], T[B]],
              applicativeEv: Applicative[F]): TraverseDecoder[F, T, B] =
    copy(decode(_).map(flattenEv.to))

}

object TraverseDecoder {

  def fromDecoder[F[_], C[_], X, A](dec: XmlDecoder[F, X, A])
                                   (implicit dfe: DecodeFromElem[F, C, X]): TraverseDecoder[F, C, A] =
    TraverseDecoder(dfe(dec.name, dec.dec, dec.filter, _))

}