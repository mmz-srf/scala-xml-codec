package ch.srf.xml

import scalaz.Monad
import scalaz.syntax.applicative._
import shapeless.{::, HList, HNil}

import scala.xml.Elem

private[xml] trait HListEncoder[F[_], C, A] {

  def apply(enc: C): Encoder[F, Elem, (Elem, A)]

}

private[xml] object HListEncoder {

  implicit def hNilEncoder[F[_]:Monad]: HListEncoder[F, HNil, HNil] =
    new HListEncoder[F, HNil, HNil] {

      override def apply(enc: HNil): Encoder[F, Elem, (Elem, HNil)] =
        Encoder.fromFunction { case (e, _) => e }

    }

  implicit def hConsEncoder[
  F[_],
  CC[_[_], _, _[_], _, _],
  C[_],
  D,
  X,
  A,
  TS <: HList,
  TA <: HList](implicit
               monadEv: Monad[F],
               toEncoder: ToXmlEncoder[CC],
               appendToElem: AppendToElem[D, C, X],
               tailEncoder: HListEncoder[F, TS, TA]): HListEncoder[F, CC[F, D, C, X, A] :: TS, C[A] :: TA] =
    new HListEncoder[F, CC[F, D, C, X, A] :: TS, C[A] :: TA] {

      override def apply(enc: CC[F, D, C, X, A] :: TS): Encoder[F, Elem, (Elem, C[A] :: TA)] =
        Encoder[F, Elem, (Elem, C[A] :: TA)] { case (parent, value) =>
          val hc :: te = enc
          val ha :: ta = value
          val he = toEncoder(hc)

          val hEnc = Encoder[F, Elem, Elem] { e =>
            val encToParent = Encoder[F, Elem, C[X]](x => appendToElem(e, x, he.descriptor.identifier).point[F])
            (encToParent ~ he.encoder).encode(ha)
          }

          (hEnc ~ tailEncoder.apply(te)).encode((parent, ta))
        }

    }


  
}