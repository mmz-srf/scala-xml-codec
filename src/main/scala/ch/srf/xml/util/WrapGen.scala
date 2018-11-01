package ch.srf.xml.util

import shapeless.{Generic, HNil, ::}

private[xml] sealed trait WrapGen[T, R] {

  def to(t: T): R

  def from(r: R): T
}

private[xml] sealed trait WrapGenLow {

  implicit def wrap[T, R](implicit gen: Generic.Aux[T, R :: HNil]): WrapGen[T, R] =
    new WrapGen[T, R] {
      override def to(t: T): R = {
        val h :: HNil = gen.to(t)
        h
      }
      override def from(r: R): T =
        gen.from(r :: HNil)
    }

}

private[xml] object WrapGen extends WrapGenLow {

  implicit def nowrap[T, R](implicit gen: Generic.Aux[T, R]): WrapGen[T, R] =
    new WrapGen[T, R] {
      override def to(t: T): R = gen.to(t)
      override def from(r: R): T = gen.from(r)
    }

}

