package ch.srf.xml.scalazext

import scalaz.syntax.std.list._
import scalaz.syntax.traverse._
import scalaz.{Applicative, NonEmptyList}

package object syntax {

  implicit class OptionSyntax[A](val o: Option[A]) extends AnyVal {

    def filterM[G[_]:Applicative](f: A => G[Boolean]): G[Option[A]] =
      o.toList.filterM(f).map(_.headOption)

  }

  implicit class NelSyntax[A](val nel: NonEmptyList[A]) extends AnyVal {

    def filterM[G[_]:Applicative](f: A => G[Boolean]): G[List[A]] =
      nel.list.toList.filterM(f)

  }

}
