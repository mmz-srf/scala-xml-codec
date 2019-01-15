package net.devkat

import scalaz.{EitherT, NonEmptyList, \/}

package object xml {

  type Errors = NonEmptyList[(List[String], String)]

  type Result[F[_], A] = EitherT[F, Errors,  A]

  type Ensure[F[_], A] = A => F[Option[String]]

}
