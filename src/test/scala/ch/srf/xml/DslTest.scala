package ch.srf.xml

import cats.data.NonEmptyList
import cats.Monad
import ch.srf.example.XmlCodec._
import java.time.LocalDate
import org.specs2.mutable.Specification
import scala.xml.{Elem, Node, PrettyPrinter}
import shapeless.HNil

object DslTest extends Specification {

  private val validXml =
    <employees>
      <employee name="Samuel Vimes" species="human">
        <rank>Commander</rank>
      </employee>
      <employee name="Fred Colon" species="human">
        <rank>Sergeant</rank>
      </employee>
      <employee name="Detritus" species="troll">
        <rank>Sergeant</rank>
        <weapon>Piecemaker</weapon>
        <weapon>Hands</weapon>
      </employee>
    </employees>


  "The schema" should {

    "successfully decode a correct XML" in {

      decodeEmployees(validXml) should beRight

    }

    "report XML validation errors" in {

      val xml =
        <employees>
          <employee species="human">
            <rank>Commander</rank>
          </employee>
          <employee name="Fred Colon" species="human">
            <rank>Sergeant</rank>
            <rank>Corporal</rank>
          </employee>
          <employee name="" species="troll">
            <rank>Sergeant</rank>
          </employee>
          <employee name="Detritus" species="troll">
            <rank></rank>
            <weapon>PieceMaker</weapon>
          </employee>
          <employee name="Nobby Nobbs" species="human">
            <rank>Corporal</rank>
          </employee>
        </employees>

      decodeEmployees(xml) should beLeft(NonEmptyList.of(
        "employees/employee[1]/@name: Attribute 'name' missing",
        "employees/employee[2]/rank: Exactly one element <rank> expected, found 2",
        "employees/employee[3]/@name: String must not be empty",
        "employees/employee[4]/rank/<text>: Text must not be empty"
      ))

    }

    "report errors from codecs" in {

      val xml =
        <employees>
          <employee name="Samuel Vimes" species="human">
            <rank>Commander</rank>
          </employee>
          <employee name="Nobby Nobbs" species="unidentified">
            <rank>Corporal</rank>
          </employee>
        </employees>

      decodeEmployees(xml) should beLeft(NonEmptyList.of(
        "employees/employee[2]/@species: Species 'unidentified' not found"
      ))

    }

    "report assertion errors" in {

      val xml =
        <employees>
          <employee name="Samuel Vimes" species="human">
            <rank>Commander</rank>
          </employee>
          <employee name="Nobby Nobbs" species="human">
            <rank>Corporal</rank>
            <weapon>Crossbow</weapon>
            <weapon>Dagger</weapon>
            <weapon>Rock</weapon>
          </employee>
        </employees>

      decodeEmployees(xml) should beLeft(NonEmptyList.of(
        "employees/employee[2]/weapon: Only 2 weapons allowed, found 3"
      ))

    }

    "support chaining codecs" in {
      import Dsl.simple.codec._
      implicit def localDateCodec[F[_]:Monad]: Codec[F, String, LocalDate] =
        Codec.from(
          Decoder.fromTryCatchNonFatal(LocalDate.parse),
          Encoder.fromFunction(_.toString)
        )

      val e = elem1("foo", attr("date").as[LocalDate])

      val today = LocalDate.now
      val xml = <foo date={today.toString}/>

      val decodeResult = e.decode(xml)
      decodeResult must beRight(today)

      val encodeResult: Elem = e.encode(today)
      encodeResult must_=== xml

    }

    "support appending decoders" in {
      import Dsl.simple.decode._
      val s = elem1("x", text ~ Decoder.fromFunction(_.length))
      s.decode(<x>xxx</x>) must beRight(3)
    }

    "support appending encoders" in {
      import Dsl.simple.encode._
      val s = elem1("x", text ~ Encoder.fromFunction("x" * (_: Int)))
      s.encode(3) must_=== <x>xxx</x>
    }

    "support appending codecs" in {
      import Dsl.simple.codec._
      val s = elem1("x", text ~ Codec.fromFunctions(_.length, "x" * (_: Int)))
      s.decode(<x>xxx</x>) must beRight(3)
      s.encode(3) must_=== <x>xxx</x>
    }

    "support modular schemas" in {
      import Dsl.simple.decode._

      final case class Foo(a: String, b: Option[String], bars: List[Bar])
      final case class Bar(c: String, d: Option[String])

      val barElem =
        elem2("bar",
          attr("c"),
          optional(attr("d"))
        ).as[Bar]

      val fooElem =
        elem3("foo",
          attr("a"),
          optional(attr("b")),
          zeroOrMore(barElem)
        ).as[Foo]

      val result: NonEmptyList[String] Either Foo = fooElem.decode(<foo></foo>)

      result must beLeft
    }

    "support mandatory, optional and potentially empty text nodes" in {
      import Dsl.simple.decode._

      val e =
        elem3("foo",
          elem1("mandatory", nonEmptyText),
          elem1("optional", optional(nonEmptyText)),
          elem1("potentiallyEmpty", text)
        )

      val invalidXml =
        <foo>
          <mandatory></mandatory>
          <optional></optional>
          <potentiallyEmpty></potentiallyEmpty>
        </foo>

      val invalidResult = e.decode(invalidXml)
      invalidResult must beLeft(NonEmptyList.of("foo/mandatory/<text>: Text must not be empty"))

      val validXml =
        <foo>
          <mandatory>Hello</mandatory>
          <optional></optional>
          <potentiallyEmpty></potentiallyEmpty>
        </foo>

      val validResult = e.decode(validXml)
      validResult must beRight("Hello" :: None :: "" :: HNil)

    }

    "correctly encode values" in {
      val prettyPrinter = new PrettyPrinter(80, 2)
      def pretty(node: Node) = prettyPrinter.format(node)

      val result = decodeEmployees(validXml).map(encodeEmployees)

      result.map(pretty) must beRight(pretty(validXml))
    }

    "support skipping elements" in {
      import Dsl.simple.decode._

      val e = elem3("parent",
        optional(elem1("foo", optional(nonEmptyText))).skip,
        zeroOrMore(elem1("bar",
          zeroOrMore(elem1("barbar", text))
        )).skip,
        oneOrMore(elem1("baz",
          oneOrMore(elem1("bazbaz", text))
        )).skip
      )

      val xml =
        <parent>
          <foo></foo>
          <bar>
            <barbar>bar 1</barbar>
            <barbar>bar 2</barbar>
          </bar>
          <baz>
            <bazbaz>baz 1</bazbaz>
            <bazbaz>baz 2</bazbaz>
          </baz>
        </parent>

      e.decode(xml) must beRight(None :: List("bar 1", "bar 2") :: NonEmptyList.of("baz 1", "baz 2") :: HNil)
    }

  }

}
