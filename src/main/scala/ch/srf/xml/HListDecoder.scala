package ch.srf.xml

import scalaz.syntax.monad._
import scalaz.{Apply, Monad}
import shapeless.{::, HList, HNil}

import scala.xml.Elem

private[xml] trait HListDecoder[F[_], C, A] {

  def apply(dec: C, e: Elem): Result[F, A]

}

private[xml] object HListDecoder {

  implicit def hNilDecoder[F[_]:Monad]: HListDecoder[F, HNil, HNil] =
    new HListDecoder[F, HNil, HNil] {

      override def apply(dec: HNil, e: Elem): Result[F, HNil] =
        Result.success(HNil)

    }

  implicit def hConsDecoder[
  F[_],
  C[_[_], _, _, _],
  D,
  X,
  A,
  TS <: HList,
  TA <: HList](implicit
               monadEv: Monad[F],
               toDecoder: ToXmlDecoder[C],
               tailDecoder: HListDecoder[F, TS, TA]): HListDecoder[F, C[F, D, X, A] :: TS, A :: TA] =
    new HListDecoder[F, C[F, D, X, A] :: TS, A :: TA] {
      override def apply(dec: C[F, D, X, A] :: TS, e: Elem): Result[F, A :: TA] = {
        val hc :: td = dec
        val hd = toDecoder(hc)
        val a = hd.getFromElem(e).monadic.flatMap(hd.dec(_).monadic).applicative
        Apply[Result[F, ?]].apply2(a, tailDecoder(td, e)) { _ :: _ }
      }
    }

}
