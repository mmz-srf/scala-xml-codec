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

      val codec = Derive[Id, Foo]

      val xml =
        <Foo id="1">
          <Bar name="bar"/>
          <Baz name="baz"/>
          <Qux name="qux1"/>
          <Qux name="qux2" value="2"/>
          <Quux name="quux1"/>
          <Quux name="quux2"/>
        </Foo>

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
