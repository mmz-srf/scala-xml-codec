package ch.srf

import scala.xml.Elem

package object xml {

  type Ensure[F[_], A] = A => F[Option[String]]

  type ElemDecoder[F[_], C[_], A] = XmlDecoder[F, String, C, Elem, A]
  type ElemEncoder[F[_], C[_], A] = XmlEncoder[F, String, C, Elem, A]
  type ElemCodec[F[_], C[_], A] = XmlCodec[F, String, C, Elem, A]

  // Alternative to scalaz.Id.Id to avoid "cyclic aliasing or subtyping" compiler error
  type Single[A] = A

}
