package ch.srf.example

import cats.data.NonEmptyList
import cats.data.Reader
import ch.srf.example.Domain._
import ch.srf.xml.{Codec, Decoder, Dsl, Encoder}
import scala.xml.Elem

object XmlCodec {

  private implicit val speciesCodec: Codec[SpeciesDirectoryReader, String, Species] =
    Codec.from(
      Decoder(speciesId => Reader(_.get(speciesId).toRight(s"Species '$speciesId' not found"))),
      Encoder(species => Reader(_.collectFirst { case (name, `species`) => name }.getOrElse("programming error")))
    )

  private val dsl = Dsl[SpeciesDirectoryReader]
  import dsl.codec._

  private val weaponElem =
    elem1("weapon", nonEmptyText).as[Weapon]

  private val employeesElem =
    elem1("employees",
      oneOrMore(elem4("employee",
        attr("name").ensure(nonEmpty),
        optional(attr("species").as[Species]),
        elem1("rank", nonEmptyText),
        zeroOrMore(weaponElem).ensure(check(_.size < 3, w => s"Only 2 weapons allowed, found ${w.size.toString}"))
      ).as[Employee])
    )

  def decodeEmployees(xml: Elem): NonEmptyList[String] Either NonEmptyList[Employee] =
    employeesElem
      .decode(xml)
      .run(speciesDirectory)

  def encodeEmployees(employees: NonEmptyList[Employee]): Elem =
    employeesElem
      .encode(employees)
      .run(speciesDirectory)
}
