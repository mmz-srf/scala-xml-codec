package ch.srf

package object xml {

  type Ensure[F[_], A] = A => F[Option[String]]

  type ElemDecoder[F[_], A] = XmlDecoder[F, ElemValue, A]
  type ElemEncoder[F[_], A] = XmlEncoder[F, ElemValue, A]
  type ElemCodec[F[_], A] = XmlCodec[F, ElemValue, A]

}
