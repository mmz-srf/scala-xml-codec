package ch.srf.xml

import scalaz.{Apply, Monad, Traverse}
import scalaz.syntax.applicative._
import scalaz.syntax.traverse._
import shapeless.{::, HList, HNil}

private[xml] trait HListEncoder[F[_], C, A] {

  def apply(enc: C): Encoder[F, ElemValue, A]

}

private[xml] object HListEncoder {

  implicit def hNilEncoder[F[_]:Monad]: HListEncoder[F, HNil, HNil] =
    new HListEncoder[F, HNil, HNil] {

      override def apply(enc: HNil): Encoder[F, ElemValue, HNil] =
        Encoder.fromFunction(_ => ElemValue.empty)

    }

  implicit def hConsEncoder[
  F[_],
  C,
  T[_],
  A,
  TS <: HList,
  TA <: HList](implicit
               monadEv: Monad[F],
               toEncoder: ToTraverseEncoder[C, F, T, A],
               tailEncoder: HListEncoder[F, TS, TA]): HListEncoder[F, C :: TS, T[A] :: TA] =
    new HListEncoder[F, C :: TS, T[A] :: TA] {

      override def apply(enc: C :: TS): Encoder[F, ElemValue, T[A] :: TA] =
        Encoder[F, ElemValue, T[A] :: TA] { value =>
          val hc :: te = enc
          val ha :: ta = value
          val he = toEncoder(hc)

          Apply[F].apply2(he.encode(ha), tailEncoder.apply(te).encode(ta))(_ append _)
        }

    }

}
