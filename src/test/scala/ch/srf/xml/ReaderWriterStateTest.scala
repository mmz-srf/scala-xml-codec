package ch.srf.xml

import cats.data.NonEmptyList
import cats.data.ReaderWriterState
import cats.syntax.all._
import org.specs2.mutable.Specification

@SuppressWarnings(Array("org.wartremover.warts.ToString"))
object ReaderWriterStateTest extends Specification {

  private type E[A] = ReaderWriterState[Unit, List[String], Option[String], A]

  private val dsl: Dsl[E] = Dsl[E]
  import dsl.decode._

  private val outerDecoder: Decoder[E, String, String] =
    Decoder(outerId => ReaderWriterState {
      (_, _) => (Nil, Some(outerId), outerId.asRight)
    })

  private val innerDecoder: Decoder[E, String, String] =
    Decoder(innerId => ReaderWriterState {
      (_, outerId) =>
        val result = outerId match {
          case None => "Outer ID not set".asLeft
          case Some(`innerId`) => innerId.asRight
          case Some(a) => s"Inner ID $innerId =/= outer ID $a".asLeft
        }
        (Nil, outerId, result)
    })

  private val elem =
    elem2("outer",
      attr("id") ~ outerDecoder,
      elem1("inner",
        attr("id") ~ innerDecoder
      )
    )

  "The decoder" should {

    "successfully decode valid XML" in {
      val xml = <outer id="a"><inner id="a"></inner></outer>
      val (_, _, result) = elem.decode(xml).run((), None).value
      result should beRight
    }

    "correctly report an error for invalid XML" in {
      val xml = <outer id="a"><inner id="b"></inner></outer>
      val (_, _, result) = elem.decode(xml).run((), None).value
      result should beLeft(NonEmptyList.of("outer/inner/@id: Inner ID b =/= outer ID a"))
    }

  }

}
