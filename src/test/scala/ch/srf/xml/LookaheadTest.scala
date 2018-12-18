package ch.srf.xml

import org.specs2.mutable.Specification
import org.specs2.scalaz.DisjunctionMatchers._
import scalaz.{NonEmptyList, Reader}
import shapeless.{::, HNil}

import scala.reflect.runtime.universe.{Type, TypeTag, typeOf}
import scala.xml.Elem

object LookaheadTest extends Specification {

  "Decoding using lookahead" should {

    import Dsl.simple.decode._

    final case class Foo(foo: String)
    final case class Bar(bar: String)
    final case class Baz(baz: String)
    final case class Qux(qux: String)

    final case class FooBarBaz(foo: Foo, bar: Option[Bar], baz: List[Baz], qux: NonEmptyList[Qux])

    def attributeIs(name: String, value: String)(e: Elem): Boolean =
      (e \ s"@$name").headOption.map(_.text).contains(value)

    val elem =
      elem4("elems",
        elem1("elem", attr("foo")).as[Foo].when(_.attribute("foo").isDefined),
        optional(elem1("elem", attr("bar")).as[Bar]).when(_.attribute("bar").isDefined),
        zeroOrMore(elem1("elem", attr("baz")).as[Baz]).when(_.attribute("baz").isDefined),
        oneOrMore(elem1("elem", attr("qux")).as[Qux]).when(_.attribute("qux").isDefined)
      ).as[FooBarBaz]

    "correctly decode valid XML" in {

      val xml =
        <elems>
          <elem foo="foo"/>
          <elem bar="bar"/>
          <elem baz="baz 1"/>
          <elem baz="baz 2"/>
          <elem qux="qux 1"/>
          <elem qux="qux 2"/>
        </elems>

      val result = FooBarBaz(
        Foo("foo"),
        Option(Bar("bar")),
        List(Baz("baz 1"), Baz("baz 2")),
        NonEmptyList(Qux("qux 1"), Qux("qux 2"))
      )

      elem.decode(xml) should be_\/-(result)

    }

    "correctly report decoding errors for cardinality Id" in {
      val xml =
        <elems>
          <elem qux="qux 1"/>
        </elems>
      elem.decode(xml) should be_-\/(NonEmptyList("elems/elem: Exactly one element <elem> expected, found 0"))
    }

    "correctly report decoding errors for cardinality Option" in {
      val xml =
        <elems>
          <elem foo="foo"/>
          <elem bar="bar 1"/>
          <elem bar="bar 2"/>
          <elem qux="qux 1"/>
        </elems>
      elem.decode(xml) should be_-\/(NonEmptyList("elems/elem: At most one element <elem> expected, found 2"))
    }

    "correctly report decoding errors for cardinality NonEmptyList" in {
      val xml =
        <elems>
          <elem foo="foo"/>
        </elems>
      elem.decode(xml) should be_-\/(NonEmptyList("elems/elem: At least one element <elem> expected"))
    }

  }

  sealed trait Soldier

  object Soldier {

    final case class Sergeant(name: String) extends Soldier

    final case class Private(id: String) extends Soldier

  }

  "Decoding to a coproduct" should {

    import Soldier._

    type WeaponDirectory = Map[String, Type]

    val weaponDirectory: WeaponDirectory =
      Map(
        "gun" -> typeOf[Sergeant],
        "sword" -> typeOf[Private]
      )

    type WeaponDirectoryReader[A] = Reader[WeaponDirectory, A]

    def isA[C](e: Elem)(implicit typeTag: TypeTag[C]): WeaponDirectoryReader[Boolean] =
      Reader(repo =>
        (e \ "@weapon")
          .headOption
          .map(_.text)
          .flatMap(repo.get)
          .contains(typeTag.tpe))

    val dsl = Dsl[WeaponDirectoryReader]
    import dsl.decode._

    val personElem =
      elem2("soldiers",

        elem1("soldier", attr("name"))
          .when(isA[Sergeant])
          .as[Sergeant],

        zeroOrMore(
          elem1("soldier", attr("id")).as[Private]
        ).when(isA[Private])

      ) ~ Decoder.fromFunction {
        v: Soldier :: List[Soldier] :: HNil =>
        val sergeant :: privates :: HNil = v
        sergeant :: privates
      }

    "work correctly" in {

      val xml =
        <soldiers>
          <soldier id="Salt" weapon="sword"/>
          <soldier name="Pepper" weapon="gun"/>
          <soldier id="Coriander" weapon="sword"/>
        </soldiers>

      val soldiers = List[Soldier](
        Sergeant("Pepper"),
        Private("Salt"),
        Private("Coriander")
      )

      personElem.decode(xml).run(weaponDirectory) must be_\/-(soldiers)

    }

  }

  "The documentation example" should {

    "work" in {

      sealed trait Child

      final case class Foo(text: String) extends Child
      final case class Bar(text: String) extends Child

      final case class FooBar(foo: Foo, bar: Bar)

      def attrEquals(name: String, value: String)(e: Elem): Boolean =
        (e \ s"@$name").headOption.map(_.text).contains(value)

      import Dsl.simple.decode._

      val elem =
        elem2("parent",
          elem1("child", text).when(attrEquals("type", "foo")).as[Foo],
          elem1("child", text).when(attrEquals("type", "bar")).as[Bar],
        ).as[FooBar]

      val xml =
        <parent>
          <child type="foo">1</child>
          <child type="bar">2</child>
        </parent>

      val foobar = FooBar(Foo("1"), Bar("2"))

      elem.decode(xml) should be_\/-(foobar)

    }

  }

}
