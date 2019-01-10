package net.devkat

package object xml {

  type Ensure[F[_], A] = A => F[Option[String]]

}
