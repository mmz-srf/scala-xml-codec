package ch.srf.xml

import ch.srf.xml.util.{CompactHList, Flatten}
import scalaz.std.string.stringInstance
import scalaz.syntax.all._
import scalaz.syntax.tag._
import scalaz.{@@, Monad, NonEmptyList, \/}

import scala.xml.Elem

final case class XmlDecoder[F[_]:Monad, D, X, A](descriptor: Descriptor[D],
                                                 dec: X => Result[F, A],
                                                 filter: X => F[Boolean],
                                                 getFromElem: Elem => F[String \/ X]) {

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

  def decodeFromParent(e: Elem)(implicit ev: GetFromElem[F, D, X]): F[NonEmptyList[String] \/ A] = {
    val decoder = Decoder[F, Elem, X](e => ev(e, descriptor.identifier, filter))
    Result.fromDisjunction(decoder.decode(e), descriptor.name).monadic.flatMap(dec(_).monadic).applicative.leftAsStrings
  }

  def when(filter: X => F[Boolean]): XmlDecoder[F, D, X, A] =
    this.copy(filter = filter)

}

object XmlDecoder {

  def collection[F[_]:Monad, C[_], D, X, A](d: XmlDecoder[F, D, X, A],
                                            cd: CardinalityDecoder[F, C, X, A])
                                           (implicit
                                           getFromElem: GetFromElem[F, D, C[X]])
  : XmlDecoder[F, D, C[X], C[A]] =
    XmlDecoder[F, D, C[X], C[A]](
      d.descriptor,
      cd.decode(d.dec, _),
      _ => true.point[F],
      e => getFromElem.apply(e, d.descriptor.identifier, d.filter)
    )

  private def textDecoder[F[_]:Monad, T]: XmlDecoder[F, Unit, String @@ T, String] =
    XmlDecoder[F, Unit, String @@ T, String](
      Descriptor.text,
      x => Result.success(x.unwrap).prependPath(Descriptor.text.name, None),
      _ => true.point[F]
    )

  def text[F[_]:Monad]: XmlDecoder[F, Unit, String @@ TextValue, String] =
    textDecoder[F, TextValue]

  def nonEmptyText[F[_]:Monad]: XmlDecoder[F, Unit, String @@ NonEmptyTextValue, String] =
    textDecoder[F, NonEmptyTextValue]

  def attr[F[_]:Monad](name: String): XmlDecoder[F, String, String @@ AttrValue, String] =
    XmlDecoder[F, String, String @@ AttrValue, String](
      Descriptor.attr(name),
      x => Result.success(x.unwrap).prependPath(Descriptor.attr(name).name, None),
      _ => true.point[F]
    )

  def elem[F[_]:Monad, CS, C, A](name: String, children: CS)
                                (implicit
                                 hListDecoder: HListDecoder[F, CS, C],
                                 compact: CompactHList[C, A]): XmlDecoder[F, String, Elem, A] = {

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
      _ => true.point[F]
    )

  }

}