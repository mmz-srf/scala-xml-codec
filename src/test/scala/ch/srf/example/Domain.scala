package ch.srf.example

import scalaz.Reader

object Domain {

  final case class Species(name: String)

  final case class Weapon(name: String)

  final case class Employee(name: String, species: Option[Species], rank: String, weapons: List[Weapon])

  type SpeciesDirectory = Map[String, Species]

  val speciesDirectory: SpeciesDirectory =
    Map(
      "human" -> Species("Human"),
      "troll" -> Species("Troll")
    )

  type SpeciesDirectoryReader[A] = Reader[SpeciesDirectory, A]

}
