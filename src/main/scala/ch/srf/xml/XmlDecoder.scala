package ch.srf.xml

import ch.srf.xml.util.{CompactHList, Flatten}
import scalaz.Id.Id
import scalaz.std.string.stringInstance
import scalaz.syntax.all._
import scalaz.syntax.tag._
import scalaz.{@@, Monad, NonEmptyList, \/}

import scala.xml.Elem

final case class XmlDecoder[F[_]:Monad, D, C[_], X, A](descriptor: Descriptor[D],
                                                       dec: C[X] => Result[F, C[A]],
                                                       filter: X => F[Boolean]) {

  def as[B](implicit dec: Decoder[F, C[A], C[B]]): XmlDecoder[F, D, C, X, B] =
    this ~ dec

  def ~[B](d: Decoder[F, C[A], C[B]]): XmlDecoder[F, D, C, X, B] =
    this.copy(
      dec = x => dec(x).monadic.flatMap(a => Result.fromDisjunction(d.decode(a), descriptor.name).monadic).applicative
    )

  def ensure(e: Ensure[F, C[A]]): XmlDecoder[F, D, C, X, A] =
    this ~ Decoder.ensure(e)

  def skip[B](implicit ev: Flatten[C[A], C[B]]): XmlDecoder[F, D, C, X, B] =
    this ~ Decoder.fromFunction(ev.to)

  def decode(x: C[X]): F[NonEmptyList[String] \/ C[A]] =
    dec(x).leftAsStrings

  def decodeFromParent(e: Elem)(implicit ev: GetFromElem[F, D, C, X]): F[NonEmptyList[String] \/ C[A]] = {
    val decoder = Decoder[F, Elem, C[X]](e => ev(e, descriptor.identifier, filter))
    Result.fromDisjunction(decoder.decode(e), descriptor.name).monadic.flatMap(dec(_).monadic).applicative.leftAsStrings
  }

  def when(filter: X => F[Boolean]): XmlDecoder[F, D, C, X, A] =
    this.copy(filter = filter)

}

object XmlDecoder {

  def collection[F[_]:Monad, C[_], D, X, A](d: XmlDecoder[F, D, Id, X, A],
                                            cd: CardinalityDecoder[F, C, X, A]): XmlDecoder[F, D, C, X, A] =
    XmlDecoder[F, D, C, X, A](
      d.descriptor,
      cd.decode(d.dec, _),
      _ => true.point[F]
    )

  private def textDecoder[F[_]:Monad, T]: XmlDecoder[F, Unit, Id, String @@ T, String] =
    XmlDecoder[F, Unit, Id, String @@ T, String](
      Descriptor.text,
      x => Result.success(x.unwrap).prependPath(Descriptor.text.name, None),
      _ => true.point[F]
    )

  def text[F[_]:Monad]: XmlDecoder[F, Unit, Id, String @@ TextValue, String] =
    textDecoder[F, TextValue]

  def nonEmptyText[F[_]:Monad]: XmlDecoder[F, Unit, Id, String @@ NonEmptyTextValue, String] =
    textDecoder[F, NonEmptyTextValue]

  def attr[F[_]:Monad](name: String): XmlDecoder[F, String, Id, String @@ AttrValue, String] =
    XmlDecoder[F, String, Id, String @@ AttrValue, String](
      Descriptor.attr(name),
      x => Result.success(x.unwrap).prependPath(Descriptor.attr(name).name, None),
      _ => true.point[F]
    )

  def elem[F[_]:Monad, CS, C, A](name: String, children: CS)
                                (implicit
                                 hListDecoder: HListDecoder[F, CS, C],
                                 compact: CompactHList[C, A]): XmlDecoder[F, String, Id, Elem, A] = {

    val descriptor = Descriptor.elem(name)

    def checkName: Decoder[F, Elem, Elem] =
      Decoder.ensure[F, Elem](EnsureOps.check(_.label === name, e => s"Found <${e.label}> instead of <$name>"))

    XmlDecoder[F, String, Id, Elem, A](
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