package ch.srf.xml

import ch.srf.xml.util.CompactHList
import scalaz.std.list.listInstance
import scalaz.std.option.optionInstance
import scalaz.{@@, Monad, NonEmptyList}
import shapeless.{::, HList, HNil}

class EncoderDsl[F[_]:Monad] extends EnsureOps {

  def optional[S, D, X, A](encoder: XmlEncoder[F, D, Single, X, A]): XmlEncoder[F, D, Option, X, A] =
    XmlEncoder.collection[F, Option, D, X, A](encoder)

  def zeroOrMore[S, D, X, A](encoder: XmlEncoder[F, D, Single, X, A]): XmlEncoder[F, D, List, X, A] =
    XmlEncoder.collection[F, List, D, X, A](encoder)

  def oneOrMore[S, D, X, A](encoder: XmlEncoder[F, D, Single, X, A]): XmlEncoder[F, D, NonEmptyList, X, A] =
    XmlEncoder.collection[F, NonEmptyList, D, X, A](encoder)

  def attr(name: String): XmlEncoder[F, String, Single, String @@ AttrValue, String] =
    XmlEncoder.attr(name)

  def text: XmlEncoder[F, Unit, Single, String @@ TextValue, String] =
    XmlEncoder.text

  def nonEmptyText: XmlEncoder[F, Unit, Single, String @@ NonEmptyTextValue, String] =
    XmlEncoder.nonEmptyText

  private def elem[SC <: HList, C, A](name: String, children: SC)
                                     (implicit
                                      dec: HListEncoder[F, SC, C],
                                      compact: CompactHList[C, A]): ElemEncoder[F, Single, A] =
    XmlEncoder.elem(name, children)

  def elem1[C1, C, A](name: String, c1: C1)
                     (implicit
                      enc: HListEncoder[F, C1 :: HNil, C],
                      compact: CompactHList[C, A]): ElemEncoder[F, Single, A] =
    elem[C1 :: HNil, C, A](name, c1 :: HNil)

  def elem2[C1, C2, C, A](name: String, c1: C1, c2: C2)
                         (implicit
                          enc: HListEncoder[F, C1 :: C2 :: HNil, C],
                          compact: CompactHList[C, A]): ElemEncoder[F, Single, A] =
    elem[C1 :: C2 :: HNil, C, A](name, c1 :: c2 :: HNil)

  def elem3[C1, C2, C3, C, A](name: String, c1: C1, c2: C2, c3: C3)
                             (implicit
                              enc: HListEncoder[F, C1 :: C2 :: C3 :: HNil, C],
                              compact: CompactHList[C, A]): ElemEncoder[F, Single, A] =
    elem[C1 :: C2 :: C3 :: HNil, C, A](name, c1 :: c2 :: c3 :: HNil)

  def elem4[C1, C2, C3, C4, C, A](name: String, c1: C1, c2: C2, c3: C3, c4: C4)
                                 (implicit
                                  enc: HListEncoder[F, C1 :: C2 :: C3 :: C4 :: HNil, C],
                                  compact: CompactHList[C, A]): ElemEncoder[F, Single, A] =
    elem[C1 :: C2 :: C3 :: C4 :: HNil, C, A](name, c1 :: c2 :: c3 :: c4 :: HNil)

  def elem5[C1, C2, C3, C4, C5, C, A](name: String, c1: C1, c2: C2, c3: C3, c4: C4, c5: C5)
                                     (implicit
                                      enc: HListEncoder[F, C1 :: C2 :: C3 :: C4 :: C5 :: HNil, C],
                                      compact: CompactHList[C, A]): ElemEncoder[F, Single, A] =
    elem[C1 :: C2 :: C3 :: C4 :: C5 :: HNil, C, A](name, c1 :: c2 :: c3 :: c4 :: c5 :: HNil)

  def elem6[C1, C2, C3, C4, C5, C6, C, A](name: String, c1: C1, c2: C2, c3: C3, c4: C4, c5: C5, c6: C6)
                                         (implicit
                                          enc: HListEncoder[F, C1 :: C2 :: C3 :: C4 :: C5 :: C6 :: HNil, C],
                                          compact: CompactHList[C, A]): ElemEncoder[F, Single, A] =
    elem[C1 :: C2 :: C3 :: C4 :: C5 :: C6 :: HNil, C, A](name, c1 :: c2 :: c3 :: c4 :: c5 :: c6 :: HNil)

  def elem7[C1, C2, C3, C4, C5, C6, C7, C, A](name: String, c1: C1, c2: C2, c3: C3, c4: C4, c5: C5, c6: C6, c7: C7)
                                             (implicit
                                              enc: HListEncoder[F, C1 :: C2 :: C3 :: C4 :: C5 :: C6 :: C7 :: HNil, C],
                                              compact: CompactHList[C, A]): ElemEncoder[F, Single, A] =
    elem[C1 :: C2 :: C3 :: C4 :: C5 :: C6 :: C7 :: HNil, C, A](name, c1 :: c2 :: c3 :: c4 :: c5 :: c6 :: c7 :: HNil)

  def elem8[C1, C2, C3, C4, C5, C6, C7, C8, C, A](name: String, c1: C1, c2: C2, c3: C3, c4: C4, c5: C5, c6: C6, c7: C7, c8: C8)
                                                 (implicit
                                                  enc: HListEncoder[F, C1 :: C2 :: C3 :: C4 :: C5 :: C6 :: C7 :: C8 :: HNil, C],
                                                  compact: CompactHList[C, A]): ElemEncoder[F, Single, A] =
    elem[C1 :: C2 :: C3 :: C4 :: C5 :: C6 :: C7 :: C8 :: HNil, C, A](name, c1 :: c2 :: c3 :: c4 :: c5 :: c6 :: c7 :: c8 :: HNil)

  def elem9[C1, C2, C3, C4, C5, C6, C7, C8, C9, C, A](name: String, c1: C1, c2: C2, c3: C3, c4: C4, c5: C5, c6: C6, c7: C7, c8: C8, c9: C9)
                                                     (implicit
                                                      enc: HListEncoder[F, C1 :: C2 :: C3 :: C4 :: C5 :: C6 :: C7 :: C8 :: C9 :: HNil, C],
                                                      compact: CompactHList[C, A]): ElemEncoder[F, Single, A] =
    elem[C1 :: C2 :: C3 :: C4 :: C5 :: C6 :: C7 :: C8 :: C9 :: HNil, C, A](name, c1 :: c2 :: c3 :: c4 :: c5 :: c6 :: c7 :: c8 :: c9 :: HNil)

  def elem10[C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, C, A](name: String, c1: C1, c2: C2, c3: C3, c4: C4, c5: C5, c6: C6, c7: C7, c8: C8, c9: C9, c10: C10)
                                                           (implicit
                                                            enc: HListEncoder[F, C1 :: C2 :: C3 :: C4 :: C5 :: C6 :: C7 :: C8 :: C9 :: C10 :: HNil, C],
                                                            compact: CompactHList[C, A]): ElemEncoder[F, Single, A] =
    elem[C1 :: C2 :: C3 :: C4 :: C5 :: C6 :: C7 :: C8 :: C9 :: C10 :: HNil, C, A](name, c1 :: c2 :: c3 :: c4 :: c5 :: c6 :: c7 :: c8 :: c9 :: c10 :: HNil)

  def elem11[C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, C11, C, A](name: String, c1: C1, c2: C2, c3: C3, c4: C4, c5: C5, c6: C6, c7: C7, c8: C8, c9: C9, c10: C10, c11: C11)
                                                                (implicit
                                                                 enc: HListEncoder[F, C1 :: C2 :: C3 :: C4 :: C5 :: C6 :: C7 :: C8 :: C9 :: C10 :: C11 :: HNil, C],
                                                                 compact: CompactHList[C, A]): ElemEncoder[F, Single, A] =
    elem[C1 :: C2 :: C3 :: C4 :: C5 :: C6 :: C7 :: C8 :: C9 :: C10 :: C11 :: HNil, C, A](name, c1 :: c2 :: c3 :: c4 :: c5 :: c6 :: c7 :: c8 :: c9 :: c10 :: c11 :: HNil)

  def elem12[C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, C11, C12, C, A](name: String, c1: C1, c2: C2, c3: C3, c4: C4, c5: C5, c6: C6, c7: C7, c8: C8, c9: C9, c10: C10, c11: C11, c12: C12)
                                                                     (implicit
                                                                      enc: HListEncoder[F, C1 :: C2 :: C3 :: C4 :: C5 :: C6 :: C7 :: C8 :: C9 :: C10 :: C11 :: C12 :: HNil, C],
                                                                      compact: CompactHList[C, A]): ElemEncoder[F, Single, A] =
    elem[C1 :: C2 :: C3 :: C4 :: C5 :: C6 :: C7 :: C8 :: C9 :: C10 :: C11 :: C12 :: HNil, C, A](name, c1 :: c2 :: c3 :: c4 :: c5 :: c6 :: c7 :: c8 :: c9 :: c10 :: c11 :: c12 :: HNil)

}
