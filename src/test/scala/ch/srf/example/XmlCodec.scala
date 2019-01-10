package ch.srf.example

import ch.srf.example.Domain._
import ch.srf.xml.{Codec, Decoder, Dsl, Encoder}
import scalaz.syntax.std.option._
import scalaz.{NonEmptyList, Reader, \/}

import scala.xml.Elem

object XmlCodec {

  private implicit val speciesCodec: Codec[SpeciesDirectoryReader, String, Species] =
    Codec.from(
      Decoder(speciesId => Reader(_.get(speciesId).\/>(s"Species '$speciesId' not found"))),
      Encoder(species => Reader(_.collectFirst { case (name, `species`) => name }.getOrElse("programming error")))
    )

  private val dsl = Dsl[SpeciesDirectoryReader]
  import dsl.codec._

  private val weaponElem =
    elem1(one("", nonEmptyText)).as[Weapon]

  private val employeesElem =
    one("employees",
      elem1(
        oneOrMore("employee", elem4(
          one("name", attr.ensure(nonEmpty)),
          optional("species", attr.as[Species]),
          one("rank", elem1(one("", nonEmptyText))),
          zeroOrMore("weapon", weaponElem)
            .ensure(check(_.size < 3, w => s"Only 2 weapons allowed, found ${w.size}"))
        ).as[Employee])
      )
    )

  def decodeEmployees(xml: Elem): NonEmptyList[String] \/ NonEmptyList[Employee] =
    employeesElem
      .decode(xml)
      .run(speciesDirectory)

  def encodeEmployees(employees: NonEmptyList[Employee]): Elem =
    employeesElem
      .encode(employees)
      .run(speciesDirectory)
}
