package net.devkat.xml

trait XmlEncoder[F[_], X, A] {
  def encode(a: A): F[X]
}
