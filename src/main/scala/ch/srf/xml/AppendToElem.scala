package ch.srf.xml

import cats.data.NonEmptyList
import cats.Eval
import cats.Foldable
import cats.syntax.all._
import scala.xml.{Attribute, Elem, Null, Text}

private[xml] sealed trait AppendToElem[D, X] {

  def apply(elem: Elem, x: X, name: D): Elem

}

private[xml] object AppendToElem {

  private def apply[D, X](f: (Elem, X, D) => Elem): AppendToElem[D, X] =
    new AppendToElem[D, X] {
      override def apply(elem: Elem, x: X, name: D): Elem =
        f(elem, x, name)
    }

  /* ----- Attributes ----- */

  implicit def attrInstance: AppendToElem[String, AttrValue] =
    apply((e, a, name) => e % Attribute(None, name, Text(a.unwrap), Null))

  implicit def attrOptionInstance[S]: AppendToElem[String, Option[AttrValue]] =
    apply ((e, a, name) => a.map(s => e % Attribute(None, name, Text(s.unwrap), Null)).getOrElse(e))

  /* ----- Text ----- */

  private def appendText[T](e: Elem)(a: String) =
    e.copy(child = e.child :+ Text(a))

  implicit def textValueInstance: AppendToElem[Unit, TextValue] =
    apply((e, a, _) => appendText(e)(a.unwrap))

  implicit def nonEmptyTextValueInstance: AppendToElem[Unit, NonEmptyTextValue] =
    apply((e, a, _) => appendText(e)(a.unwrap))

  implicit def optionalNonEmptyTextValueInstance: AppendToElem[Unit, Option[NonEmptyTextValue]] =
    apply((e, a, _) => a.map(_.unwrap).map(appendText(e)).getOrElse(e))

  /* ----- Elements ----- */

  implicit def elemInstance: AppendToElem[String, Elem] =
    apply((e, a, _) => e.copy(child = a +: e.child))

  def elemsInstance[Cy[_]:Foldable]: AppendToElem[String, Cy[Elem]] =
    apply((parent: Elem, a: Cy[Elem], _: String) => a.foldRight(parent.pure[Eval])((e, p) => p.value.copy(child = e +: p.value.child).pure[Eval]).value)

  implicit def elemOptionInstance: AppendToElem[String, Option[Elem]] =
    elemsInstance[Option]

  implicit def elemListInstance: AppendToElem[String, List[Elem]] =
    elemsInstance[List]

  implicit def elemNelInstance: AppendToElem[String, NonEmptyList[Elem]] =
    elemsInstance[NonEmptyList]

}
