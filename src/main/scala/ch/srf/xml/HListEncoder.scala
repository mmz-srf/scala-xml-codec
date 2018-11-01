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

  implicit def hConsEncoder[F[_], C[_[_], _, _, _], D, X, A, TS <: HList, TA <: HList](implicit
                                                                                       monadEv: Monad[F],
                                                                                       toEncoder: ToXmlEncoder[C],
                                                                                       appendToElem: AppendToElem[D, X],
                                                                                       tailEncoder: HListEncoder[F, TS, TA]): HListEncoder[F, C[F, D, X, A] :: TS, A :: TA] =
    new HListEncoder[F, C[F, D, X, A] :: TS, A :: TA] {

      override def apply(enc: C[F, D, X, A] :: TS): Encoder[F, Elem, (Elem, A :: TA)] =
        Encoder[F, Elem, (Elem, A :: TA)] { case (parent, value) =>
          val hc :: te = enc
          val ha :: ta = value
          val he = toEncoder(hc)

          val hEnc = Encoder[F, Elem, Elem] { e =>
            val encToParent = Encoder[F, Elem, X](x => appendToElem(e, x, he.descriptor.identifier).point[F])
            (encToParent ~ he.encoder).encode(ha)
          }

          (hEnc ~ tailEncoder.apply(te)).encode((parent, ta))
        }

    }


  
}