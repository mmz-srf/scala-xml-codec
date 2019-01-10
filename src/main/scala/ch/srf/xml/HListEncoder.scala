package ch.srf.xml

import scalaz.{Apply, Monad}
import scalaz.syntax.applicative._
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
  C[_[_], _, _],
  X,
  A,
  TS <: HList,
  TA <: HList](implicit
               monadEv: Monad[F],
               toEncoder: ToXmlEncoder[C],
               tailEncoder: HListEncoder[F, TS, TA]): HListEncoder[F, C[F, ElemValue, A] :: TS, A :: TA] =
    new HListEncoder[F, C[F, ElemValue, A] :: TS, A :: TA] {

      override def apply(enc: C[F, ElemValue, A] :: TS): Encoder[F, ElemValue, A :: TA] =
        Encoder[F, ElemValue, A :: TA] { value =>
          val hc :: te = enc
          val ha :: ta = value
          val he = toEncoder(hc)

          Apply[F].apply2(he.encode(ha), tailEncoder.apply(te).encode(ta))(_ append _)
        }

    }


  
}