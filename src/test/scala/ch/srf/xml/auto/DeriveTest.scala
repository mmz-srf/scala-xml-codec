package ch.srf.xml.auto

import java.time.LocalDate

import ch.srf.xml.Codecs._
import ch.srf.xml.PrettyPrint
import org.specs2.mutable.Specification
import org.specs2.scalaz.DisjunctionMatchers.be_\/-
import scalaz.Id.Id
import scalaz.{@@, NonEmptyList}

object DeriveTest extends Specification {

  "Derived codecs" should {

    "correctly decode and encode XML" in {

      sealed trait Name

      final case class Foo(id: Option[Int],
                           name: Option[String @@ Name],
                           bar: Bar,
                           baz: Option[Baz],
                           quxs: List[Qux],
                           quuxs: NonEmptyList[Quux])

      final case class Bar(date: LocalDate)

      final case class Baz(name: String)

      final case class Qux(name: String, value: Option[Int])

      final case class Quux(name: String)

      val codec = Derive[Id, Foo](_.toLowerCase)

      val xml =
        <foo id="1">
          <bar date="2018-11-28"/>
          <baz name="baz"/>
          <qux name="qux1"/>
          <qux name="qux2" value="2"/>
          <quux name="quux1"/>
          <quux name="quux2"/>
        </foo>

      val foo =
        Foo(
          id = Some(1),
          name = None,
          bar = Bar(LocalDate.parse("2018-11-28")),
          baz = Some(Baz("baz")),
          quxs = List(Qux("qux1", None), Qux("qux2", Some(2))),
          quuxs = NonEmptyList(Quux("quux1"), Quux("quux2"))
        )

      codec.decode(xml) should be_\/-(foo)

      PrettyPrint(codec.encode(foo)) should_=== PrettyPrint(xml)
    }

  }

}
