package net.devkat.xml

trait XmlCodec[F[_], X, A] extends XmlDecoder[F, X, A] with XmlEncoder[F, X, A]
