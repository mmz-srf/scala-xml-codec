package ch.srf.xml

import scalaz.{Apply, Monad}
import shapeless.{::, HList, HNil}

private[xml] trait HListDecoder[F[_], C, A] {

  def apply(dec: C, e: ElemValue): Result[F, A]

}

private[xml] object HListDecoder {

  implicit def hNilDecoder[F[_]:Monad]: HListDecoder[F, HNil, HNil] =
    new HListDecoder[F, HNil, HNil] {

      override def apply(dec: HNil, e: ElemValue): Result[F, HNil] =
        Result.success(HNil)

    }

  implicit def hConsDecoder[
  F[_],
  C[_[_], _, _],
  A,
  TS <: HList,
  TA <: HList](implicit
               monadEv: Monad[F],
               toDecoder: ToXmlDecoder[C],
               tailDecoder: HListDecoder[F, TS, TA]): HListDecoder[F, C[F, ElemValue, A] :: TS, A :: TA] =
    new HListDecoder[F, C[F, ElemValue, A] :: TS, A :: TA] {
      override def apply(dec: C[F, ElemValue, A] :: TS, e: ElemValue): Result[F, A :: TA] = {
        val hc :: td = dec
        val hd = toDecoder(hc)
        val a = hd.dec(e)
        Apply[Result[F, ?]].apply2(a, tailDecoder(td, e)) { _ :: _ }
      }
    }

}
