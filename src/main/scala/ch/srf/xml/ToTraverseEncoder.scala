package ch.srf.xml

import scalaz.Applicative
import scalaz.Id.Id

private[xml] trait ToTraverseEncoder[C, F[_], T[_], A] {
  def apply(c: C): TraverseEncoder[F, T, A]
}

private[xml] object ToTraverseEncoder {

  implicit def codecInstance[F[_], T[_], X, A]: ToTraverseEncoder[TraverseCodec[F, T, X, A], F, T, A] =
    new ToTraverseEncoder[TraverseCodec[F, T, X, A], F, T, A] {
      override def apply(c: TraverseCodec[F, T, X, A]): TraverseEncoder[F, T, A] =
        c.encoder
    }

  implicit def decoderInstance[F[_], T[_], A]: ToTraverseEncoder[TraverseEncoder[F, T, A], F, T, A] =
    new ToTraverseEncoder[TraverseEncoder[F, T, A], F, T, A] {
      override def apply(e: TraverseEncoder[F, T, A]): TraverseEncoder[F, T, A] =
        e
    }

  implicit def xmlEncoderInstance[F[_]:Applicative, X, A](implicit append: AppendToElem[X])
  : ToTraverseEncoder[XmlEncoder[F, X, A], F, Id, A] =
    new ToTraverseEncoder[XmlEncoder[F, X, A], F, Id, A] {
      override def apply(c: XmlEncoder[F, X, A]): TraverseEncoder[F, Id, A] =
        TraverseEncoder.fromEncoder(c)
    }

  implicit def xmlCodecInstance[F[_]:Applicative, X, A](implicit append: AppendToElem[X])
  : ToTraverseEncoder[XmlCodec[F, X, A], F, Id, A] =
    new ToTraverseEncoder[XmlCodec[F, X, A], F, Id, A] {
      override def apply(c: XmlCodec[F, X, A]): TraverseEncoder[F, Id, A] =
        TraverseEncoder.fromEncoder(c.encoder)
    }

}
/*
object Test {

  private val dsl = Dsl.simple
  import dsl.codec._
  import shapeless.{::, HNil}
  import scalaz.std.option.optionInstance

  def enc[C]: HListEncoder[scalaz.Id.Id,TraverseCodec[scalaz.Id.Id,Option,AttrValue,String] :: shapeless.HNil,C] =
    HListEncoder.hConsEncoder[
      scalaz.Id.Id,
      TraverseCodec[scalaz.Id.Id,Option,AttrValue,String],
      Option,
  X,
  A,
  HNil,
  HNil
      ]

  private val employeesElem =
    elem1("employees",
      oneOrMore(elem1("employee",
        optional(attr("species"))
      ))
    )



}
*/
