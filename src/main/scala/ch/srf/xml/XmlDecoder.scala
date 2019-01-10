package ch.srf.xml

import ch.srf.xml.util.{CompactHList, Flatten}
import scalaz.Id.Id
import scalaz.std.string.stringInstance
import scalaz.syntax.all._
import scalaz.syntax.tag._
import scalaz.{@@, Monad, NonEmptyList, \/}

import scala.xml.Elem

final case class XmlDecoder[F[_], X, A](dec: X => Result[F, A],
                                        filter: X => Result[F, Boolean]) {

  def as[B](implicit dec: Decoder[F, A, B], monadEv: Monad[F]): XmlDecoder[F, X, B] =
    this ~ dec

  def ~[B](d: Decoder[F, A, B])(implicit monadEv: Monad[F]): XmlDecoder[F, X, B] =
    this.copy(
      dec = x => dec(x).monadic.flatMap(a => Result.fromDisjunction(d.decode(a)).monadic).applicative,
    )

  def ensure(e: Ensure[F, A])(implicit monadEv: Monad[F]): XmlDecoder[F, X, A] =
    this ~ Decoder.ensure(e)

  def skip[B](implicit flattenEv: Flatten[A, B], monadEv: Monad[F]): XmlDecoder[F, X, B] =
    this ~ Decoder.fromFunction(flattenEv.to)

  def decode(e: Elem)(implicit ev: ElemValue =:= X): F[NonEmptyList[String] \/ A] =
    dec(ev(ElemValue.fromElem(e))).leftAsStrings

  private[xml] def decFromParent(e: Elem)(implicit ev: ElemValue =:= X): Result[F, A] =
    dec(ev(ElemValue.fromElem(e)))

  def decodeFromParent(e: Elem)(implicit ev: ElemValue =:= X): F[NonEmptyList[String] \/ A] =
    decFromParent(e).leftAsStrings

  def when(predicate: XmlDecoder[F, X, Boolean]): XmlDecoder[F, X, A] =
    XmlDecoder[F, X, A](
      dec,
      predicate.dec
    )

}

object XmlDecoder {

  private def nopFilter[F[_]:Monad, A]: A => Result[F, Boolean] =
    _ => Result.success(true)

  def collection[F[_]:Monad, C[_], X, A](name: String,
                                         dec: XmlDecoder[F, X, A])
                                        (implicit dfe: DecodeFromElem[F, C, X]): XmlDecoder[F, ElemValue, C[A]] =
    XmlDecoder(dfe(name, dec.dec, dec.filter, _), nopFilter)

  def text[F[_]:Monad]: XmlDecoder[F, TextValue, String] =
    XmlDecoder[F, TextValue, String](
      x => Result.success(x.value),
      nopFilter
    )

  def nonEmptyText[F[_]:Monad]: XmlDecoder[F, NonEmptyTextValue, String] =
    XmlDecoder[F, NonEmptyTextValue, String](
      x => Result.success(x.value),
      nopFilter
    )

  def attr[F[_]: Monad]: XmlDecoder[F, AttrValue, String] =
    XmlDecoder[F, AttrValue, String](
      x => Result.success(x.value),
      nopFilter
    )

  def elem[F[_]:Monad, CS, C, A](children: CS)
                                (implicit
                                 hListDecoder: HListDecoder[F, CS, C],
                                 compact: CompactHList[C, A]): XmlDecoder[F, ElemValue, A] = {
/*
    def checkName: Decoder[F, Elem, Elem] =
      Decoder.ensure[F, Elem](EnsureOps.check(_.label === name, e => s"Found <${e.label}> instead of <$name>"))

    XmlDecoder[F, ElemValue, A](
      e => Result
        .fromDisjunction(checkName.decode(e))
        .monadic
        .flatMap(_ => hListDecoder(children, e).prependPath(name).monadic)
        .map(compact.to)
        .applicative,
      nopFilter
    )
    */

    XmlDecoder[F, ElemValue, A](
      e => hListDecoder(children, e)
        .monadic
        .map(compact.to)
        .applicative,
      nopFilter
    )

  }

}