package ch.srg.xml

import org.specs2.mutable.Specification
import org.specs2.scalaz.DisjunctionMatchers._
import scalaz.std.list.listMonoid
import scalaz.syntax.either._
import scalaz.{NonEmptyList, ReaderWriterState}

object ReaderWriterStateTest extends Specification {

  private type E[A] = ReaderWriterState[Unit, List[String], Option[String], A]

  private val dsl: Dsl[E] = Dsl[E]
  import dsl.decode._

  private val outerDecoder: Decoder[E, String, String] =
    Decoder(outerId => ReaderWriterState {
      (_, _) => (Nil, outerId.right, Some(outerId))
    })

  private val innerDecoder: Decoder[E, String, String] =
    Decoder(innerId => ReaderWriterState {
      (_, outerId) =>
        val result = outerId match {
          case None => "Outer ID not set".left
          case Some(`innerId`) => innerId.right
          case Some(a) => s"Inner ID $innerId =/= outer ID $a".left
        }
        (Nil, result, outerId)
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
      val (_, result, _) = elem.decode(xml).run((), None)
      result should be_\/-
    }

    "correctly report an error for invalid XML" in {
      val xml = <outer id="a"><inner id="b"></inner></outer>
      val (_, result, _) = elem.decode(xml).run((), None)
      result should be_-\/(NonEmptyList("outer/inner/@id: Inner ID b =/= outer ID a"))
    }

  }

}
