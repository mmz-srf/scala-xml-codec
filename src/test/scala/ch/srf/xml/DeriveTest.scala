package ch.srf.xml

import org.specs2.mutable.Specification
import org.specs2.scalaz.DisjunctionMatchers._
import scalaz.Id.Id
import scalaz.NonEmptyList

object DeriveTest extends Specification {

  "Derived codecs" should {

    "correctly decode and encode XML" in {

      final case class Foo(id: Option[Int],
                           name: Option[String],
                           bar: Bar,
                           baz: Option[Baz],
                           quxs: List[Qux],
                           quuxs: NonEmptyList[Quux])

      final case class Bar(name: String)

      final case class Baz(name: String)

      final case class Qux(name: String, value: Option[Int])

      final case class Quux(name: String)

      val codec = Derive[Id, Foo](_.toLowerCase)

      val xml =
        <foo id="1">
          <bar name="bar"/>
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
          bar = Bar("bar"),
          baz = Some(Baz("baz")),
          quxs = List(Qux("qux1", None), Qux("qux2", Some(2))),
          quuxs = NonEmptyList(Quux("quux1"), Quux("quux2"))
        )

      codec.decode(xml) should be_\/-(foo)

      PrettyPrint(codec.encode(foo)) should_=== PrettyPrint(xml)
    }

  }

}
