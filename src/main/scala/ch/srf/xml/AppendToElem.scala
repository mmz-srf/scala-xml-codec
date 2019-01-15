package ch.srf.xml

import scalaz.std.list.listInstance
import scalaz.std.option.optionInstance
import scalaz.syntax.foldable._
import scalaz.syntax.tag._
import scalaz.{@@, Foldable, NonEmptyList}

import scala.xml.{Attribute, Elem, Null, Text}

private[xml] sealed trait AppendToElem[X] {

  def apply(x: X, name: String): ElemValue

}

private[xml] object AppendToElem {

  private def apply[X](f: (X, String) => ElemValue): AppendToElem[X] =
    new AppendToElem[X] {
      override def apply(x: X, name: String): ElemValue =
        f(x, name)
    }

  implicit def attrInstance: AppendToElem[AttrValue] =
    apply((a, name) => ElemValue.empty.copy(attributes = Map(name -> a.value)))

  implicit def textValueInstance: AppendToElem[TextValue] =
    apply((a, _) => ElemValue.empty.copy(text = Some(a.value)))

  implicit def nonEmptyTextValueInstance: AppendToElem[NonEmptyTextValue] =
    apply((a, _) => ElemValue.empty.copy(text = Some(a.value)))

  implicit def elemInstance: AppendToElem[ElemValue] =
    apply((a, name) => ElemValue.empty.copy(elements = a.appendTo(<dummy/>.copy(label = name)) :: Nil))

}