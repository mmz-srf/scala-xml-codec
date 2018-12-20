package ch.srf.xml

import ch.srf.xml.util.{CompactHList, Flatten}
import scalaz.Id.Id
import scalaz.std.string.stringInstance
import scalaz.syntax.all._
import scalaz.syntax.tag._
import scalaz.{@@, Monad, NonEmptyList, \/}

import scala.xml.Elem

final case class XmlDecoder[F[_]:Monad, D, X, A](descriptor: Descriptor[D],
                                                 dec: X => Result[F, A],
                                                 filter: X => Result[F, Boolean],
                                                 getFromElem: Elem => Result[F, X]) {

  def as[B](implicit dec: Decoder[F, A, B]): XmlDecoder[F, D, X, B] =
    this ~ dec

  def ~[B](d: Decoder[F, A, B]): XmlDecoder[F, D, X, B] =
    this.copy(
      dec = x => dec(x).monadic.flatMap(a => Result.fromDisjunction(d.decode(a), descriptor.name).monadic).applicative
    )

  def ensure(e: Ensure[F, A]): XmlDecoder[F, D, X, A] =
    this ~ Decoder.ensure(e)

  def skip[B](implicit ev: Flatten[A, B]): XmlDecoder[F, D, X, B] =
    this ~ Decoder.fromFunction(ev.to)

  def decode(x: X): F[NonEmptyList[String] \/ A] =
    dec(x).leftAsStrings

  def decodeFromParent(e: Elem): F[NonEmptyList[String] \/ A] =
    getFromElem(e).monadic.flatMap(dec(_).monadic).applicative.leftAsStrings

  def when(predicate: XmlDecoder[F, D, X, Boolean])
          (implicit
           getFromElem: GetFromElem[F, D, Id, X]): XmlDecoder[F, D, X, A] =
    XmlDecoder[F, D, X, A](
      descriptor,
      dec,
      predicate.dec,
      getFromElem(_, descriptor.identifier, predicate.dec)
    )

}

object XmlDecoder {

  private def nopFilter[F[_]:Monad, A]: A => Result[F, Boolean] =
    _ => Result.success(true)

  def collection[F[_]:Monad, C[_], D, X, A](d: XmlDecoder[F, D, X, A],
                                            cd: CardinalityDecoder[F, C, X, A])
                                           (implicit
                                            getFromElem: GetFromElem[F, D, C, X]): XmlDecoder[F, D, C[X], C[A]] =
    XmlDecoder[F, D, C[X], C[A]](
      d.descriptor,
      cd.decode(d.dec, _),
      nopFilter,
      getFromElem(_, d.descriptor.identifier, d.filter)
    )

  def when[F[_]:Monad, D, X, A](predicate: XmlDecoder[F, D, X, Boolean], d: XmlDecoder[F, D, X, A])
                               (implicit
                                getFromElem: GetFromElem[F, D, Id, X]): XmlDecoder[F, D, X, A] =
    XmlDecoder[F, D, X, A](
      d.descriptor,
      d.dec,
      predicate.dec,
      getFromElem(_, d.descriptor.identifier, predicate.dec)
    )

  private def textDecoder[
  F[_]: Monad,
  T](implicit
     getFromElem: GetFromElem[F, Unit, Id, String @@ T]): XmlDecoder[F, Unit, String @@ T, String] = {

    val descriptor = Descriptor.text

    XmlDecoder[F, Unit, String @@ T, String](
      descriptor,
      x => Result.success(x.unwrap).prependPath(Descriptor.text.name, None),
      nopFilter,
      getFromElem.apply(_, descriptor.identifier, nopFilter)
    )
  }

  def text[F[_]:Monad]: XmlDecoder[F, Unit, String @@ TextValue, String] =
    textDecoder[F, TextValue]

  def nonEmptyText[F[_]:Monad]: XmlDecoder[F, Unit, String @@ NonEmptyTextValue, String] =
    textDecoder[F, NonEmptyTextValue]

  def attr[F[_]: Monad](name: String)
                       (implicit getFromElem: GetFromElem[F, String, Id, String @@ AttrValue])
  : XmlDecoder[F, String, String @@ AttrValue, String] = {

    val descriptor = Descriptor.attr(name)

    XmlDecoder[F, String, String @@ AttrValue, String](
      descriptor,
      x => Result.success(x.unwrap).prependPath(Descriptor.attr(name).name, None),
      nopFilter,
      getFromElem(_, descriptor.identifier, nopFilter)
    )
  }

  def elem[F[_]:Monad, CS, C, A](name: String, children: CS)
                                (implicit
                                 hListDecoder: HListDecoder[F, CS, C],
                                 compact: CompactHList[C, A],
                                 getFromElem: GetFromElem[F, String, Id, Elem]): XmlDecoder[F, String, Elem, A] = {

    val descriptor = Descriptor.elem(name)

    def checkName: Decoder[F, Elem, Elem] =
      Decoder.ensure[F, Elem](EnsureOps.check(_.label === name, e => s"Found <${e.label}> instead of <$name>"))

    XmlDecoder[F, String, Elem, A](
      descriptor,
      e => Result
        .fromDisjunction(checkName.decode(e), descriptor.name)
        .monadic
        .flatMap(_ => hListDecoder(children, e).prependPath(descriptor.name, None).monadic)
        .map(compact.to)
        .applicative,
      nopFilter,
      getFromElem(_, descriptor.identifier, nopFilter)
    )

  }

}