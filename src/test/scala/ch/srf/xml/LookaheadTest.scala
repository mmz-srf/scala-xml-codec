package ch.srf.xml

import org.specs2.matcher.{MatchResult, NoTypedEqual}
import org.specs2.mutable.Specification
import org.specs2.scalaz.DisjunctionMatchers._
import scalaz.std.string.stringInstance
import scalaz.syntax.applicative._
import scalaz.syntax.either._
import scalaz.syntax.equal._
import scalaz.{NonEmptyList, Reader}
import scalaz.Id.Id
import shapeless.{::, HNil}

import scala.reflect.runtime.universe.{Type, TypeTag, typeOf}
import scala.xml.Elem

object LookaheadTest extends Specification with NoTypedEqual {

  "Decoding using lookahead" should {

    import Dsl.simple.decode._

    final case class Foo(foo: String)
    final case class Bar(bar: String)
    final case class Baz(baz: String)
    final case class Qux(qux: String)

    final case class FooBarBaz(foo: Foo, bar: Option[Bar], baz: List[Baz], qux: NonEmptyList[Qux])

    def discriminator(attrName: String): XmlDecoder[scalaz.Id.Id, String, Elem, Boolean] =
      elem1("elem", optional(attr(attrName))) ~ Decoder.fromFunction(_.isDefined)

    def attributeIs(name: String, value: String)(e: Elem): Boolean =
      (e \ s"@$name").headOption.map(_.text).contains(value)

    type R[A] = Result[Id, A]

    val elem =
      elem4("elems",
        when(elem1("elem", attr("foo")).as[Foo], discriminator("foo")),
        optional(when(elem1("elem", attr("bar")).as[Bar], discriminator("bar"))),
        zeroOrMore(when(elem1("elem", attr("baz")).as[Baz], discriminator("baz"))),
        oneOrMore(when(elem1("elem", attr("qux")).as[Qux], discriminator("qux")))
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

    val dsl = Dsl[WeaponDirectoryReader]
    import dsl.decode._

    def isA[C](implicit typeTag: TypeTag[C]): ElemDecoder[WeaponDirectoryReader, Boolean] = {

      implicit val dec: Decoder[WeaponDirectoryReader, String, Boolean] =
        Decoder(s => Reader(_.get(s).contains(typeTag.tpe).right))

      elem1("soldier", attr("weapon")).as[Boolean]
    }

    val personElem =
      elem2("soldiers",

        when(elem1("soldier", attr("name")).as[Sergeant], isA[Sergeant]),

        zeroOrMore(when(elem1("soldier", attr("id")).as[Private], isA[Private]))

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

      def attrEquals(name: String, value: String): ElemDecoder[scalaz.Id.Id, Boolean] =
        elem1("child", attr(name)) ~ Decoder.fromFunction(_ === value)

      val elem =
        elem2("parent",
          when(elem1("child", text).as[Foo], attrEquals("type", "foo")),
          when(elem1("child", text).as[Bar], attrEquals("type", "bar"))
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
