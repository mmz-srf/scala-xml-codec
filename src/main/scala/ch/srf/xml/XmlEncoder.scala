package ch.srf.xml

import ch.srf.xml.util.{CompactHList, Flatten}
import scalaz.syntax.all._
import scalaz.{Monad, Traverse}

final case class XmlEncoder[F[_]:Monad, X, A](encode: A => F[X]) {

  def as[B](implicit enc: Encoder[F, A, B]): XmlEncoder[F, X, B] =
    this ~ enc

  def ~[B](enc: Encoder[F, A, B]): XmlEncoder[F, X, B] =
    XmlEncoder[F, X, B]((Encoder(encode) ~ enc).encode)

  def skip[B](implicit ev: Flatten[A, B]): XmlEncoder[F, X, B] =
    this ~ Encoder.fromFunction(ev.from)

}

object XmlEncoder {

  def collection[F[_]:Monad, C[_]:Traverse, X, A](name: String, encoder: XmlEncoder[F, X, A])
                                                 (implicit append: AppendToElem[X]): XmlEncoder[F, ElemValue, C[A]] =
    XmlEncoder(_
      .traverse(encoder.encode(_))
      .map(_.foldLeft(ElemValue.empty) { case (e, a) => append(e, a, name) }))

  def text[F[_]:Monad]: XmlEncoder[F, TextValue, String] =
    XmlEncoder(TextValue(_).point[F])

  def nonEmptyText[F[_]:Monad]: XmlEncoder[F, NonEmptyTextValue, String] =
    XmlEncoder(NonEmptyTextValue(_).point[F])

  def attr[F[_]:Monad]: XmlEncoder[F, AttrValue, String] =
    XmlEncoder(AttrValue(_).point[F])

  def elem[F[_]:Monad, CS, C, A](children: CS)
                                (implicit
                                 hListEncoder: HListEncoder[F, CS, C],
                                 compact: CompactHList[C, A]): XmlEncoder[F, ElemValue, A] = {

    def compactEncoder: Encoder[F, C, A] =
      Encoder.fromFunction[F, C, A](compact.from)

    new XmlEncoder[F, ElemValue, A]((hListEncoder.apply(children) ~ compactEncoder).encode)

  }

}