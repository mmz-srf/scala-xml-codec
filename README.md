# Scala XML Codec

XML validation and binding library.

## Features

* Validate XML structure
* Assertions on nodes and node collections
* Decode XML
  * As nested HLists (default)
  * As arbitrary data types by using custom decoders
  * Using effects (decoders can target an arbitrary monad)
* Encode XML
* Modular schemas

## Limitations

* No support for namespaces
* No support for validating the order of nodes
* No support for individual text nodes (only `Elem.text` is supported)

## Examples

* Test case to demonstrate usage:
  * [Domain classes](src/test/scala/ch/srf/example/Domain.scala)
  * [XML codec](src/test/scala/ch/srf/example/XmlCodec.scala)
  * [Test class](src/test/scala/ch/srf/xml/DslTest.scala)

## Usage

### Schema builder API

[Schema builder API](src/main/scala/ch/srf/xml/Dsl.scala)

#### Cardinalities

Supported cardinalities for node collections:

    …              -> A
    optional(…)    -> Option[A]
    oneOrMore(…)   -> NonEmptyList[A]
    zeroOrMore(…)  -> List[A]

#### Elements

    elem1("name", child)
    elem2("name", child1, child2)
    …

Attributes, text and child elements are declared as children:

    elem3("name",
      attr(…),
      elem1("child", …),
      text
    )

#### Attributes

    attribute("name")

Mandatory/optional attributes:

    attr("name")            -> String
    optional(attr("name"))  -> Option[String]

#### Text

There are two flavours of handling text:

By combining `nonEmptyText` with the `one` and `optional` cardinalities. In this case it is assured that no empty text values are emitted:

    nonEmptyText            -> String
    optional(nonEmptyText)  -> Option[String]

By using `text` directly. In this case an empty text value are emitted if the parent element doesn't contain any text:

    text                    -> String

The schema `nonEmptyText` is equivalent to `text.ensure(nonEmpty)`.

### Assertions

Assertions can be made using `ensure`:

    attr("name").ensure(nonEmpty)
    attr("name").ensure(mustEqual("Sam"))

Assertions can be applied to collections:

      oneOrMore(elem1("employee",
        zeroOrMore(
          elem1("tool", text)
        ).ensure(check(_.size < 3, _ => "Only 2 tools allowed"))
      ))

#### Custom assertions

Custom assertions can be implemented in this fashion.
It is possible to use an effect `F`; see section on codecs for details.

    def nonEmpty[F[_]:Applicative]: Ensure[F, String] =
      _.isEmpty.option("String must not be empty").point[F]

## Modes (encode, decode, codec)

When using the schema builder DSL, you have to decide what you want to use your schema for: encoding, decoding, or both. This is done by importing the corresponding variant of the DSL:

    import Dsl.simple.decode._
    import Dsl.simple.encode._
    import Dsl.simple.codec._

For decode/encode schemas, only decoders/encoders for the involved types need to be provided. For codec schemas, both decoders and encoders are required.

This approach allows the compiler to show an error when a decoder/encoder is missing in a specific location. The alternative would be using mode-agnostic schemas and assembling the decoder/encoder implicitly when it is required, but this would lead to incomprehensible implicit-not-found error messages.


## Decoding XML

In this example we use the simple schema which targets the `scalaz.Id.Id` monad:

    // Get access to the schema builder API
    import Dsl.simple.codec._

    final case class Foo(a: String, b: Option[String], bars: List[Bar])
    final case class Bar(c: String, d: Option[String])

    val schema =
      elem3("foo",
        attr("a"),
        optional(attr("b")),
        zeroOrMore(elem2("bar",
          attr("c"),
          optional(attr("d"))
        ))
      )

    val result: NonEmptyList[String] \/ Foo =
      schema.decode(<foo>…</foo>)
        .map {
          case a :: b :: barElems :: HNil =>
            val bars = barElems map {
              case c :: d :: HNil => Bar(c, d)
            }
            Foo(a, b, bars)
        }

## Composing schemas

Schemas can be composed by including other schemas. A codec schema can be included in decoder and encoder schemas, but not the other way around.

    import Dsl.simple.codec._

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
        zeroOrMore(barElem) // Include other schema
      ).as[Foo]

    val result: NonEmptyList[String] \/ Foo = fooElem.decode(<foo></foo>)

## Codecs

Text nodes, attributes, elements and collections thereof can be decoded/encoded to/from custom types. Any error messages generated during decoding are prepended with the XML path, which makes it easier to locate the source of the error.

### Usage

Pass a codec implicitly:

    attr("name").as[A]

Pass a codec explicitly:

    attr("name") ~ codecForA

Codecs can be chained:

    attr("foo")
      .as[LocalDate]                  // decode to LocalDate
      .as[LocalDate @@ StartDate]     // apply tag

### Codecs for generic representations

If the target type of a schema is a `HList` that is the generic representation of a certain type (e.g. case class) `A`, a codec for `A` is provided out of the box, courtesy of [shapeless](https://github.com/milessabin/shapeless):

    final case class Bar(c: String, d: Option[String])

    val barElem = elem2("bar", attr("c"), optional(attr("d"))).as[Bar]

### Implementing custom codecs

A codec targets an effect monad `F`, which makes it usable only in schemas supporting this effect type.

#### Implementing a codec for arbitrary effects

Example for a decoder for a `scalaz.Tag` type:

    sealed trait Name

    implicit def nameCodec[F[_]]: Codec[F, String, String @@ Name] =
      Codec.fromFunctions(
        decode = Tag.of[Name](_),
        encode = _.unwrap
      )

Usage:

    attr("name").as[String @@ Name]

If you need error handling, use `Decoder.fromTryCatchNonFatal` or `Decoder.fromDisjunction` and return a disjunction (`NonEmptyList[String] \/ A`):

    implicit def myCodec[F[_]]: Codec[F, String, Foo] =
      Codec.from(
        Decoder.fromDisjunction(x => if (canDecode(x)) \/-(…) else -\/(NonEmptyList("An error occurred"))),
        Encoder.fromFunction(_.unwrap)
      )

#### Implementing a codec targeting a specific effect

Example for a decoder targeting a `scalaz.Reader`:

    type EnvReader[A] = Reader[Env, A]

    def findVideoFormat(env: Env, name: String): Option[VideoFormat] = ???
    def getVideoFormatName(env: Env, videoFormat: VideoFormat): String = ???

    implicit def videoFormatCodec: Codec[EnvReader, String, VideoFormat] =
      Codec.from(
        Decoder.fromEitherT(name => EitherT(Reader { env =>
          val videoFormat: Option[VideoFormat] = findVideoFormat(env, name)
          videoFormat.\/>(s"Video format $name not found")
        })),
        Encoder.fromEffect(f => Reader(env => getVideoFormatName(env, f)))
      )

Usage:

    attr("name").as[VideoFormat]

Decoding XML targeting the `EnvReader` effect monad:

    // Create a schema
    val mySchema = new ch.srg.xml.Schema[EnvReader]

    // Get access to the schema builder API
    import mySchema.codec._

    val schema = …

    schema
      .decode(xml)
      .run(env) // Execute the effect

#### Conditional Decoding

It is possible to conditionally decode nodes using the `when` method. In the following example, the element is only decoded if it has an `id` attribute, otherwise it is ignored:

    elem1("foo", …)
      .when(_.attribute("id").isDefined)
      .as[FooWithId]

A typical use-case is decoding different types from elements with the same name. In this case the `when` function can be used as discriminator:

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

      elem.decode(xml) // FooBar(Foo("1"), Bar("2"))

The `when` function accepts a function which returns an effectful value `F[Boolean]`, so it is possible to use effects (e.g. read values from an environment using a `Reader`).
