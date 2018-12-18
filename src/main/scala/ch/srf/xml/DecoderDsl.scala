package ch.srf.xml

import ch.srf.xml.util.CompactHList
import scalaz.{@@, Monad, NonEmptyList}
import shapeless.{::, HList, HNil}

class DecoderDsl[F[_]:Monad] extends EnsureOps {

  def optional[S, D, X, A](decoder: XmlDecoder[F, D, Single, X, A]): XmlDecoder[F, D, Option, X, A] =
    XmlDecoder.collection[F, Option, D, X, A](decoder, CardinalityDecoder.option)

  def zeroOrMore[S, D, X, A](decoder: XmlDecoder[F, D, Single, X, A]): XmlDecoder[F, D, List, X, A] =
    XmlDecoder.collection[F, List, D, X, A](decoder, CardinalityDecoder.list)

  def oneOrMore[S, D, X, A](decoder: XmlDecoder[F, D, Single, X, A]): XmlDecoder[F, D, NonEmptyList, X, A] =
    XmlDecoder.collection[F, NonEmptyList, D, X, A](decoder, CardinalityDecoder.nel)

  def attr(name: String): XmlDecoder[F, String, Single, String @@ AttrValue, String] =
    XmlDecoder.attr(name)

  def text: XmlDecoder[F, Unit, Single, String @@ TextValue, String] =
    XmlDecoder.text

  def nonEmptyText: XmlDecoder[F, Unit, Single, String @@ NonEmptyTextValue, String] =
    XmlDecoder.nonEmptyText

  private def elem[SC <: HList, C, A](name: String, children: SC)
                                     (implicit
                                      dec: HListDecoder[F, SC, C],
                                      compact: CompactHList[C, A]): ElemDecoder[F, Single, A] =
    XmlDecoder.elem(name, children)

  def elem1[C1, C, A](name: String, c1: C1)
                     (implicit
                      dec: HListDecoder[F, C1 :: HNil, C],
                      compact: CompactHList[C, A]): ElemDecoder[F, Single, A] =
    elem[C1 :: HNil, C, A](name, c1 :: HNil)

  def elem2[C1, C2, C, A](name: String, c1: C1, c2: C2)
                         (implicit
                          dec: HListDecoder[F, C1 :: C2 :: HNil, C],
                          compact: CompactHList[C, A]): ElemDecoder[F, Single, A] =
    elem[C1 :: C2 :: HNil, C, A](name, c1 :: c2 :: HNil)

  def elem3[C1, C2, C3, C, A](name: String, c1: C1, c2: C2, c3: C3)
                             (implicit
                              dec: HListDecoder[F, C1 :: C2 :: C3 :: HNil, C],
                              compact: CompactHList[C, A]): ElemDecoder[F, Single, A] =
    elem[C1 :: C2 :: C3 :: HNil, C, A](name, c1 :: c2 :: c3 :: HNil)

  def elem4[C1, C2, C3, C4, C, A](name: String, c1: C1, c2: C2, c3: C3, c4: C4)
                                 (implicit
                                  dec: HListDecoder[F, C1 :: C2 :: C3 :: C4 :: HNil, C],
                                  compact: CompactHList[C, A]): ElemDecoder[F, Single, A] =
    elem[C1 :: C2 :: C3 :: C4 :: HNil, C, A](name, c1 :: c2 :: c3 :: c4 :: HNil)

  def elem5[C1, C2, C3, C4, C5, C, A](name: String, c1: C1, c2: C2, c3: C3, c4: C4, c5: C5)
                                     (implicit
                                      dec: HListDecoder[F, C1 :: C2 :: C3 :: C4 :: C5 :: HNil, C],
                                      compact: CompactHList[C, A]): ElemDecoder[F, Single, A] =
    elem[C1 :: C2 :: C3 :: C4 :: C5 :: HNil, C, A](name, c1 :: c2 :: c3 :: c4 :: c5 :: HNil)

  def elem6[C1, C2, C3, C4, C5, C6, C, A](name: String, c1: C1, c2: C2, c3: C3, c4: C4, c5: C5, c6: C6)
                                         (implicit
                                          dec: HListDecoder[F, C1 :: C2 :: C3 :: C4 :: C5 :: C6 :: HNil, C],
                                          compact: CompactHList[C, A]): ElemDecoder[F, Single, A] =
    elem[C1 :: C2 :: C3 :: C4 :: C5 :: C6 :: HNil, C, A](name, c1 :: c2 :: c3 :: c4 :: c5 :: c6 :: HNil)

  def elem7[C1, C2, C3, C4, C5, C6, C7, C, A](name: String, c1: C1, c2: C2, c3: C3, c4: C4, c5: C5, c6: C6, c7: C7)
                                             (implicit
                                              dec: HListDecoder[F, C1 :: C2 :: C3 :: C4 :: C5 :: C6 :: C7 :: HNil, C],
                                              compact: CompactHList[C, A]): ElemDecoder[F, Single, A] =
    elem[C1 :: C2 :: C3 :: C4 :: C5 :: C6 :: C7 :: HNil, C, A](name, c1 :: c2 :: c3 :: c4 :: c5 :: c6 :: c7 :: HNil)

  def elem8[C1, C2, C3, C4, C5, C6, C7, C8, C, A](name: String, c1: C1, c2: C2, c3: C3, c4: C4, c5: C5, c6: C6, c7: C7, c8: C8)
                                                 (implicit
                                                  dec: HListDecoder[F, C1 :: C2 :: C3 :: C4 :: C5 :: C6 :: C7 :: C8 :: HNil, C],
                                                  compact: CompactHList[C, A]): ElemDecoder[F, Single, A] =
    elem[C1 :: C2 :: C3 :: C4 :: C5 :: C6 :: C7 :: C8 :: HNil, C, A](name, c1 :: c2 :: c3 :: c4 :: c5 :: c6 :: c7 :: c8 :: HNil)

  def elem9[C1, C2, C3, C4, C5, C6, C7, C8, C9, C, A](name: String, c1: C1, c2: C2, c3: C3, c4: C4, c5: C5, c6: C6, c7: C7, c8: C8, c9: C9)
                                                     (implicit
                                                      dec: HListDecoder[F, C1 :: C2 :: C3 :: C4 :: C5 :: C6 :: C7 :: C8 :: C9 :: HNil, C],
                                                      compact: CompactHList[C, A]): ElemDecoder[F, Single, A] =
    elem[C1 :: C2 :: C3 :: C4 :: C5 :: C6 :: C7 :: C8 :: C9 :: HNil, C, A](name, c1 :: c2 :: c3 :: c4 :: c5 :: c6 :: c7 :: c8 :: c9 :: HNil)

  def elem10[C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, C, A](name: String, c1: C1, c2: C2, c3: C3, c4: C4, c5: C5, c6: C6, c7: C7, c8: C8, c9: C9, c10: C10)
                                                           (implicit
                                                            dec: HListDecoder[F, C1 :: C2 :: C3 :: C4 :: C5 :: C6 :: C7 :: C8 :: C9 :: C10 :: HNil, C],
                                                            compact: CompactHList[C, A]): ElemDecoder[F, Single, A] =
    elem[C1 :: C2 :: C3 :: C4 :: C5 :: C6 :: C7 :: C8 :: C9 :: C10 :: HNil, C, A](name, c1 :: c2 :: c3 :: c4 :: c5 :: c6 :: c7 :: c8 :: c9 :: c10 :: HNil)

  def elem11[C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, C11, C, A](name: String, c1: C1, c2: C2, c3: C3, c4: C4, c5: C5, c6: C6, c7: C7, c8: C8, c9: C9, c10: C10, c11: C11)
                                                                (implicit
                                                                 dec: HListDecoder[F, C1 :: C2 :: C3 :: C4 :: C5 :: C6 :: C7 :: C8 :: C9 :: C10 :: C11 :: HNil, C],
                                                                 compact: CompactHList[C, A]): ElemDecoder[F, Single, A] =
    elem[C1 :: C2 :: C3 :: C4 :: C5 :: C6 :: C7 :: C8 :: C9 :: C10 :: C11 :: HNil, C, A](name, c1 :: c2 :: c3 :: c4 :: c5 :: c6 :: c7 :: c8 :: c9 :: c10 :: c11 :: HNil)

  def elem12[C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, C11, C12, C, A](name: String, c1: C1, c2: C2, c3: C3, c4: C4, c5: C5, c6: C6, c7: C7, c8: C8, c9: C9, c10: C10, c11: C11, c12: C12)
                                                                    (implicit
                                                                     dec: HListDecoder[F, C1 :: C2 :: C3 :: C4 :: C5 :: C6 :: C7 :: C8 :: C9 :: C10 :: C11 :: C12 :: HNil, C],
                                                                     compact: CompactHList[C, A]): ElemDecoder[F, Single, A] =
    elem[C1 :: C2 :: C3 :: C4 :: C5 :: C6 :: C7 :: C8 :: C9 :: C10 :: C11 :: C12 :: HNil, C, A](name, c1 :: c2 :: c3 :: c4 :: c5 :: c6 :: c7 :: c8 :: c9 :: c10 :: c11 :: c12 :: HNil)


}
