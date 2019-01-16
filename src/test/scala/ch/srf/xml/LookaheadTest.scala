package ch.srf.xml

import org.specs2.matcher.NoTypedEqual
import org.specs2.mutable.Specification
import org.specs2.scalaz.DisjunctionMatchers._
import scalaz.Id.Id
import scalaz.std.string.stringInstance
import scalaz.syntax.either._
import scalaz.syntax.equal._
import scalaz.{NonEmptyList, Reader}
import shapeless.{::, HNil}

import scala.reflect.runtime.universe.{Type, TypeTag, typeOf}
import scala.xml.Elem

object LookaheadTest extends Specification with NoTypedEqual {

  "Decoding using lookahead" should {

    import Dsl.simple.decode._

    final case class Foo(foo: String)
    final case class Bar(bar: String)
    final case class Baz(baz: String)
    final case class Qux(qux: String, value: String)

    final case class FooBarBaz(foo: Foo, bar: Option[Bar], baz: List[Baz], qux: NonEmptyList[Qux])

    def discriminator(attrName: String): ElemDecoder[Id, Boolean] =
      elem1("elem", optional(attr(attrName))) ~ Decoder.fromFunction(_.isDefined)

    type R[A] = Result[Id, A]

    val elem =
      elem4("elems",
        elem1("elem", attr("foo")).as[Foo].when(discriminator("foo")),
        optional(elem1("elem", attr("bar")).as[Bar].when(discriminator("bar"))),
        zeroOrMore(elem1("elem", attr("baz")).as[Baz].when(discriminator("baz"))),
        oneOrMore(elem2("elem", attr("qux"), attr("value")).as[Qux].when(discriminator("qux")))
      ).as[FooBarBaz]

    "correctly decode valid XML" in {

      val xml =
        <elems>
          <elem foo="foo"/>
          <elem bar="bar"/>
          <elem baz="baz 1"/>
          <elem baz="baz 2"/>
          <elem qux="qux 1" value="a"/>
          <elem qux="qux 2" value="b"/>
        </elems>

      val result = FooBarBaz(
        Foo("foo"),
        Option(Bar("bar")),
        List(Baz("baz 1"), Baz("baz 2")),
        NonEmptyList(Qux("qux 1", "a"), Qux("qux 2", "b"))
      )

      elem.decode(xml) should be_\/-(result)

    }

    "correctly report decoding errors for cardinality Id" in {
      val xml =
        <elems>
          <elem qux="qux 1" value="a"/>
        </elems>
      elem.decode(xml) should be_-\/(NonEmptyList("elems: Exactly one element <elem> expected, found 0"))
    }

    "correctly report decoding errors for cardinality Option" in {
      val xml =
        <elems>
          <elem foo="foo"/>
          <elem bar="bar 1"/>
          <elem bar="bar 2"/>
          <elem qux="qux 1" value="a"/>
        </elems>
      elem.decode(xml) should be_-\/(NonEmptyList("elems: At most one element <elem> expected, found 2"))
    }

    "correctly report decoding errors for cardinality NonEmptyList" in {
      val xml =
        <elems>
          <elem foo="foo"/>
        </elems>
      elem.decode(xml) should be_-\/(NonEmptyList("elems: At least one element <elem> expected"))
    }

    "emit the correct position for decoding errors" in {
      val xml =
        <elems>
          <elem foo="foo"/>
          <elem bar="bar 1"/>
          <elem baz="baz"/>
          <elem qux="qux 1"/>
        </elems>
      elem.decode(xml) should be_-\/(NonEmptyList("elems/elem[4]: Attribute 'value' missing"))
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

    val dsl = Dsl[WeaponDirectoryReader]
    import dsl.decode._

    def isA[C](implicit typeTag: TypeTag[C]): ElemDecoder[WeaponDirectoryReader, Boolean] = {

      implicit val dec: Decoder[WeaponDirectoryReader, String, Boolean] =
        Decoder(s => Reader(_.get(s).contains(typeTag.tpe).right))

      elem1("soldier", attr("weapon")).as[Boolean]
    }

    val personElem =
      elem2("soldiers",
        elem1("soldier", attr("name")).as[Sergeant].when(isA[Sergeant]),
        zeroOrMore(elem1("soldier", attr("id")).as[Private].when(isA[Private]))
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

      import Dsl.simple.decode._

      def attrEquals(name: String, value: String): ElemDecoder[Id, Boolean] =
        elem1("child", attr(name)) ~ Decoder.fromFunction(_ === value)

      val elem =
        elem2("parent",
          elem1("child", text).as[Foo].when(attrEquals("type", "foo")),
          elem1("child", text).as[Bar].when(attrEquals("type", "bar"))
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
