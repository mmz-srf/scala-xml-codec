package ch.srf.xml

import scalaz.std.list.listInstance
import scalaz.std.option.optionInstance
import scalaz.syntax.foldable._
import scalaz.syntax.tag._
import scalaz.{@@, Foldable, NonEmptyList}

import scala.xml.{Attribute, Elem, Null, Text}

private[xml] sealed trait AppendToElem[X] {

  def apply(elem: ElemValue, x: X, name: String): ElemValue

}

private[xml] object AppendToElem {

  private def apply[X](f: (ElemValue, X, String) => ElemValue): AppendToElem[X] =
    new AppendToElem[X] {
      override def apply(elem: ElemValue, x: X, name: String): ElemValue =
        f(elem, x, name)
    }

  implicit def attrInstance: AppendToElem[AttrValue] =
    apply((e, a, name) => e.copy(attributes = e.attributes + (name -> a.value)))

  implicit def textValueInstance: AppendToElem[TextValue] =
    apply((e, a, _) => e.copy(text = Some(a.value)))

  implicit def nonEmptyTextValueInstance: AppendToElem[NonEmptyTextValue] =
    apply((e, a, _) => e.copy(text = Some(a.value)))

  implicit def elemInstance: AppendToElem[ElemValue] =
    apply((e, a, name) => e.copy(elements = e.elements :+ a.appendTo(<dummy/>.copy(label = name))))

}