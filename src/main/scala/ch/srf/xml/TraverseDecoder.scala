package ch.srf.xml

final case class TraverseDecoder[F[_], T[_], X, A](decoder: XmlDecoder[F, X, A])
                                                  (implicit dfe: DecodeFromElem[F, T, X]) {

  def decode(e: ElemValue): Result[F, T[A]] =
    dfe(decoder.name, decoder.dec, decoder.filter, e)

}
