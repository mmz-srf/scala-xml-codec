package ch.srg.xml

import scalaz.std.anyVal.intInstance
import scalaz.std.option.optionEqual
import scalaz.std.string.stringInstance
import scalaz.std.tuple._
import scalaz.syntax.foldable1._
import scalaz.{Equal, NonEmptyList, Show}

private[xml] final case class Path(elems: NonEmptyList[(String, Option[Int])]) {

  def prepend(name: String, position: Option[Int]): Path =
    Path((s"$name", position) <:: elems)

  def updatePos(pos: Int): Path = {
    val (name, _) = elems.head
    Path(NonEmptyList.nel((name, Some(pos)), elems.tail))
  }

}

private[xml] object Path {

  implicit def showInstance: Show[Path] =
    new Show[Path] {
      override def shows(path: Path): String =
        path.elems.map { case (name, pos) => name + pos.fold("")("[" + _.toString + "]") }.foldLeft1(_ + "/" + _)
    }

  implicit def equalInstance: Equal[Path] =
    Equal.equalBy[Path, NonEmptyList[(String, Option[Int])]](_.elems)

}
