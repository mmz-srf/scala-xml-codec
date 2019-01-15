package ch.srf.xml

import scalaz.syntax.traverse._
import scalaz.{Applicative, Traverse}

final case class TraverseEncoder[F[_]:Applicative, T[_]:Traverse, X, A](encoder: XmlEncoder[F, X, A])
                                                                       (implicit append: AppendToElem[X]) {

  def encode(as: T[A]): F[ElemValue] =
    as.traverse(encoder.enc(_)).map(_.map(append(_, encoder.name)).fold)

}
