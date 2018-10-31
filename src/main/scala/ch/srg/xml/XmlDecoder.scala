package ch.srg.xml

import ch.srg.xml.util.{CompactHList, Flatten}
import scalaz.std.string.stringInstance
import scalaz.syntax.all._
import scalaz.syntax.tag._
import scalaz.{@@, Monad, NonEmptyList, \/}

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
        outer.dec(x).monadic.flatMap(a => Result.fromDisjunction(d.decode(a), descriptor.name).monadic).applicative
    }

  def ensure(e: Ensure[F, A]): XmlDecoder[F, D, X, A] =
    this ~ Decoder.ensure(e)

  def skip[B](implicit ev: Flatten[A, B]): XmlDecoder[F, D, X, B] =
    this ~ Decoder.fromFunction(ev.to)

  def decode(x: X): F[NonEmptyList[String] \/ A] =
    dec(x).leftAsStrings

  def decodeFromParent(e: Elem)(implicit ev: GetFromElem[D, X]): F[NonEmptyList[String] \/ A] = {
    val decoder = Decoder.fromDisjunction[F, Elem, X](e => ev(e, descriptor.identifier))
    Result.fromDisjunction(decoder.decode(e), descriptor.name).monadic.flatMap(dec(_).monadic).applicative.leftAsStrings
  }

}

object XmlDecoder {
/*
  private def prependPath[F[_]:Monad, X, A](decoder: Decoder[F, X, A], name: String) =
    Decoder(decoder.decode _ andThen (_.prependPath(name, None)))
*/
  def collection[F[_]:Monad, C[_], D, X, A](d: XmlDecoder[F, D, X, A],
                                            cd: CardinalityDecoder[F, C, X, A]): XmlDecoder[F, D, C[X], C[A]] =
    new XmlDecoder[F, D, C[X], C[A]] {

      override def descriptor: Descriptor[D] =
        d.descriptor

      override def dec(x: C[X]): Result[F, C[A]] =
        cd.decode(d.descriptor.name, d.dec, x)

    }


  private def textDecoder[F[_]:Monad, T]: XmlDecoder[F, Unit, String @@ T, String] =
    new XmlDecoder[F, Unit, String @@ T, String] {

      override def descriptor: Descriptor[Unit] =
        Descriptor.text

      override def dec(x: String @@ T): Result[F, String] =
        Result.success(x.unwrap).prependPath(descriptor.name, None)

    }

  def text[F[_]:Monad]: XmlDecoder[F, Unit, String @@ TextValue, String] =
    textDecoder[F, TextValue]

  def nonEmptyText[F[_]:Monad]: XmlDecoder[F, Unit, String @@ NonEmptyTextValue, String] =
    textDecoder[F, NonEmptyTextValue]

  def attr[F[_]:Monad](name: String): XmlDecoder[F, String, String @@ AttrValue, String] =
    new XmlDecoder[F, String, String @@ AttrValue, String] {

      override def dec(x: String @@ AttrValue): Result[F, String] =
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
          .fromDisjunction(checkName.decode(e), descriptor.name)
          .monadic
          .flatMap(_ => hListDecoder(children, e).prependPath(descriptor.name, None).monadic)
          .map(compact.to)
          .applicative

    }

}