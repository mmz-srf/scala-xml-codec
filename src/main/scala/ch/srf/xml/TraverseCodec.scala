package ch.srf.xml

final case class TraverseCodec[F[_], T[_], X, A](decoder: TraverseDecoder[F, T, X, A],
                                                 encoder: TraverseEncoder[F, T, X, A])
