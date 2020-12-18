package ch.srf

import scalaz.{@@, NonEmptyList}

import scala.xml.Elem

package object xml {

  type Ensure[F[_], A] = A => F[Option[String]]

  type AttrDecoder[F[_], A] = XmlDecoder[F, String, String @@ AttrValue, A]
  type AttrEncoder[F[_], A] = XmlEncoder[F, String, String @@ AttrValue, A]
  type AttrCodec[F[_], A] = XmlCodec[F, String, String @@ AttrValue, A]

  type ElemDecoder[F[_], A] = XmlDecoder[F, String, Elem, A]
  type ElemEncoder[F[_], A] = XmlEncoder[F, String, Elem, A]
  type ElemCodec[F[_], A] = XmlCodec[F, String, Elem, A]

}
