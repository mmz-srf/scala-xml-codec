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
  C,
  T[_],
  A,
  TS <: HList,
  TA <: HList](implicit
               monadEv: Monad[F],
               toDecoder: ToTraverseDecoder[C, F, T, A],
               tailDecoder: HListDecoder[F, TS, TA]): HListDecoder[F, C :: TS, T[A] :: TA] =
    new HListDecoder[F, C :: TS, T[A] :: TA] {
      override def apply(dec: C :: TS, e: ElemValue): Result[F, T[A] :: TA] = {
        val hc :: td = dec
        val hd = toDecoder(hc)
        val a = hd.decode(e)
        Apply[Result[F, ?]].apply2(a, tailDecoder(td, e)) { _ :: _ }
      }
    }

}
