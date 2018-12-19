package ch.srf.xml

import scalaz.Id.Id
import scalaz.std.list.listInstance
import scalaz.std.option.optionInstance
import scalaz.syntax.foldable._
import scalaz.syntax.tag._
import scalaz.{@@, Foldable, NonEmptyList}

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

  implicit def attrInstance: AppendToElem[String, String @@ AttrValue] =
    apply[String, String @@ AttrValue]((e, a, name) => e % Attribute(None, name, Text(a.unwrap), Null))

  implicit def attrOptionInstance[S]: AppendToElem[String, Option[String @@ AttrValue]] =
    apply((e, a, name) => a.map(s => e % Attribute(None, name, Text(s.unwrap), Null)).getOrElse(e))

  /* ----- Text ----- */

  private def appendText[T](e: Elem)(a: String @@ T) =
    e.copy(child = e.child :+ Text(a.unwrap))

  implicit def textValueInstance: AppendToElem[Unit, String @@ TextValue] =
    apply((e, a, _) => appendText(e)(a))

  implicit def nonEmptyTextValueInstance: AppendToElem[Unit, String @@ NonEmptyTextValue] =
    apply((e, a, _) => appendText(e)(a))

  implicit def optionalNonEmptyTextValueInstance: AppendToElem[Unit, Option[String @@ NonEmptyTextValue]] =
    apply((e, a, _) => a.map(appendText(e)).getOrElse(e))

  /* ----- Elements ----- */

  implicit def elemInstance: AppendToElem[String, Elem] =
    apply((e, a, _) => e.copy(child = a +: e.child))

  def elemsInstance[C[_]:Foldable]: AppendToElem[String, C[Elem]] =
    apply((parent, a, _) => a.foldRight(parent)((e, p) => p.copy(child = e +: p.child)))

  implicit def elemOptionInstance: AppendToElem[String, Option[Elem]] =
    elemsInstance[Option]

  implicit def elemListInstance: AppendToElem[String, List[Elem]] =
    elemsInstance[List]

  implicit def elemNelInstance: AppendToElem[String, NonEmptyList[Elem]] =
    elemsInstance[NonEmptyList]

}