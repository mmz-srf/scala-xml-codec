package ch.srf.xml

import ch.srf.xml.util.Flatten
import scalaz.syntax.all._
import scalaz.{Applicative, Monad, Traverse}

final case class TraverseEncoder[F[_], T[_]:Traverse, A](encode: T[A] => F[ElemValue]) {

  def ~[B](e: Encoder[F, T[A], T[B]])
          (implicit monadEv: Monad[F]): TraverseEncoder[F, T, B] =
    copy(encode = (Encoder(encode) ~ e).encode)

  def skip[B](implicit flattenEv: Flatten[T[A], T[B]]): TraverseEncoder[F, T, B] =
    copy(encode = a => encode(flattenEv.from(a)))

}

object TraverseEncoder {

  def fromEncoder[F[_]:Applicative, T[_]:Traverse, X, A](encoder: XmlEncoder[F, X, A])
                 (implicit append: AppendToElem[X]): TraverseEncoder[F, T, A] =

    TraverseEncoder[F, T, A](
      _.traverse(encoder.enc(_)).map(_.map(append(_, encoder.name)).fold)
    )

}