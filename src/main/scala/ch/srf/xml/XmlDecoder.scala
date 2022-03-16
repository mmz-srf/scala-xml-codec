package ch.srf.xml

import cats.Contravariant
import cats.data.NonEmptyList
import cats.Monad
import cats.Semigroup
import cats.syntax.all._
import ch.srf.xml.util.{CompactHList, Flatten}
import scala.xml.Elem

sealed abstract class XmlDecoder[F[_]:Monad, D, X, A] {
  outer =>

  def descriptor: Descriptor[D]

  def dec(x: X): Result[F, A]

  def as[B](implicit dec: Decoder[F, A, B]): XmlDecoder[F, D, X, B] =
    this ~ dec

  def ~[B](d: Decoder[F, A, B]): XmlDecoder[F, D, X, B] =
    new XmlDecoder[F, D, X, B] {

      override def descriptor: Descriptor[D] =
        outer.descriptor

      override def dec(x: X): Result[F, B] =
        outer.dec(x).monadic.flatMap(a => Result.fromEither(d.decode(a), descriptor.name).monadic).applicative
    }

  def ensure(e: Ensure[F, A]): XmlDecoder[F, D, X, A] =
    this ~ Decoder.ensure(e)

  def skip[B](implicit ev: Flatten[A, B]): XmlDecoder[F, D, X, B] =
    this ~ Decoder.fromFunction(ev.to)

  def decode(x: X): F[NonEmptyList[String] Either A] =
    dec(x).leftAsStrings

  def decodeFromParent(e: Elem)(implicit ev: GetFromElem[D, X]): F[NonEmptyList[String] Either A] = {
    val decoder = Decoder.fromEither[F, Elem, X](e => ev(e, descriptor.identifier))
    Result.fromEither(decoder.decode(e), descriptor.name).monadic.flatMap(dec(_).monadic).applicative.leftAsStrings
  }

  def getFromElem(e: Elem)(implicit ev: GetFromElem[D, X]): String Either X =
    ev(e, descriptor.identifier)

  final def xContravariant: XmlDecoder.XContravariant[F, D, A, X] = new XmlDecoder.XContravariant(this)
}

object XmlDecoder {

  sealed class XContravariant[F[_], D, A, X](val xmlDecoder: XmlDecoder[F, D, X, A])

  object XContravariant {
    implicit def contravariant[F[_]: Monad, D, A]: Contravariant[XContravariant[F, D, A, *]] = new Contravariant[XContravariant[F, D, A, *]] {
      override def contramap[B, C](fa: XContravariant[F,D,A,B])(f: C => B): XContravariant[F,D,A,C] = new XContravariant(
        new XmlDecoder[F, D, C, A] {
          override def descriptor: Descriptor[D] = fa.xmlDecoder.descriptor

          override def dec(x: C): Result[F,A] = fa.xmlDecoder.dec(f(x))

        }
      )
    }
  }
  def collection[F[_]:Monad, C[_], D, X, A](d: XmlDecoder[F, D, X, A],
                                            cd: CardinalityDecoder[F, C, X, A]): XmlDecoder[F, D, C[X], C[A]] =
    new XmlDecoder[F, D, C[X], C[A]] {

      override def descriptor: Descriptor[D] =
        d.descriptor

      override def dec(x: C[X]): Result[F, C[A]] =
        cd.decode(d.dec, x)

    }


  private def textDecoder[F[_]:Monad]: XmlDecoder[F, Unit, String, String] =
    new XmlDecoder[F, Unit, String, String] {

      override def descriptor: Descriptor[Unit] =
        Descriptor.text

      override def dec(x: String): Result[F, String] =
        Result.success(x).prependPath(descriptor.name, None)
    }

  def text[F[_]:Monad]: XmlDecoder[F, Unit, TextValue, String] =
    textDecoder[F].xContravariant.contramap[TextValue](_.unwrap).xmlDecoder

  def nonEmptyText[F[_]:Monad]: XmlDecoder[F, Unit, NonEmptyTextValue, String] =
    textDecoder[F].xContravariant.contramap[NonEmptyTextValue](_.unwrap).xmlDecoder

  def attr[F[_]:Monad](name: String): XmlDecoder[F, String, AttrValue, String] =
    new XmlDecoder[F, String, AttrValue, String] {

      override def dec(x: AttrValue): Result[F, String] =
        Result.success(x.unwrap).prependPath(descriptor.name, None)

      override def descriptor: Descriptor[String] =
        Descriptor.attr(name)

    }

  def elem[F[_]:Monad, CS, C, A](name: String, children: CS)
                                (implicit
                                 hListDecoder: HListDecoder[F, CS, C],
                                 compact: CompactHList[C, A]): XmlDecoder[F, String, Elem, A] =
    new XmlDecoder[F, String, Elem, A] {

      private def checkName: Decoder[F, Elem, Elem] =
        Decoder.ensure[F, Elem](EnsureOps.check(_.label === name, e => s"Found <${e.label}> instead of <$name>"))

      override def descriptor: Descriptor[String] =
        Descriptor.elem(name)

      override def dec(e: Elem): Result[F, A] =
        Result
          .fromEither(checkName.decode(e), descriptor.name)
          .monadic
          .flatMap(_ => hListDecoder(children, e).prependPath(descriptor.name, None).monadic)
          .map(compact.to)
          .applicative

    }

  def or[F[_] : Monad, D : Semigroup, X, A](one: XmlDecoder[F,D,X,A],two: XmlDecoder[F,D,X,A], descriptorSeparator: D): XmlDecoder[F,D,X,A] = new XmlDecoder[F,D,X,A]{
    override def dec(x: X): Result[F,A] = Result(
      one.dec(x).value.flatMap(
        _.fold(
          errors => two.dec(x).value.map( _.leftMap( errors |+| _ )),
          _.pure[Either[Result.Errors, *]].pure[F]
        )
      )
    )

    override def descriptor: Descriptor[D] = Descriptor.or(one.descriptor,two.descriptor,descriptorSeparator)

    @SuppressWarnings(Array("org.wartremover.warts.ImplicitParameter"))
    override def decodeFromParent(e: Elem)(implicit ev: GetFromElem[D,X]): F[NonEmptyList[String] Either A] = one.decodeFromParent(e).flatMap(
      _.fold(
        errors => two.decodeFromParent(e).map(_.leftMap( errors |+| _)),
        _.pure[Either[NonEmptyList[String], *]].pure[F]
      )
    )

    @SuppressWarnings(Array("org.wartremover.warts.ImplicitParameter"))
    override def getFromElem(e: Elem)(implicit ev: GetFromElem[D,X]): String Either X =
      one.getFromElem(e).orElse(two.getFromElem(e))
  }

}
