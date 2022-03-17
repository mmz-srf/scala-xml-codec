package ch.srf.xml

import cats.data.NonEmptyList
import cats.{Eq, Show}

private[xml] final case class Path(elems: NonEmptyList[(String, Option[Int])]) {

  def prepend(name: String, position: Option[Int]): Path =
    Path(NonEmptyList.of((s"$name", position)) ::: elems)

  def updatePos(pos: Int): Path = {
    val (name, _) = elems.head
    Path(NonEmptyList.of((name, Some(pos)), elems.tail: _*))
  }

}

private[xml] object Path {

  implicit def showInstance: Show[Path] =
    new Show[Path] {
      override def show(path: Path): String =
        path.elems.map { case (name, pos) => name + pos.fold("")("[" + _.toString + "]") }.reduceLeft(_ + "/" + _)
    }

  implicit def equalInstance: Eq[Path] =
    Eq.by[Path, NonEmptyList[(String, Option[Int])]](_.elems)

}
