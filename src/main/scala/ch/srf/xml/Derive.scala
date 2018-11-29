package ch.srf.xml

import scalaz.{@@, Monad, NonEmptyList, Traverse, \/}
import shapeless.ops.hlist.{Mapper, Reverse}
import shapeless.ops.record.Keys
import shapeless.{::, Generic, HList, HNil, LabelledGeneric, Poly1}
import scalaz.std.anyVal.intInstance
import scalaz.std.list.listInstance
import scalaz.std.option.optionInstance
import scalaz.syntax.equal._

import scala.annotation.tailrec
import scala.reflect.ClassTag
import scala.xml.Elem

trait Derive[F[_], A] {

  def apply: ElemCodec[F, A]

}

object Derive {

  def apply[F[_], A](implicit deriveEv: Derive[F, A]): ElemCodec[F, A] =
    deriveEv.apply

  trait DeriveCodec[F[_], D, X, A] {
    def apply(name: String): XmlCodec[F, D, X, A]
  }

  trait DeriveCodecLow {

    implicit def attrInstance[F[_]:Monad, A](implicit codec: Codec[F, String, A]): DeriveCodec[F, String, String @@ AttrValue, A] =
      new DeriveCodec[F, String, String @@ AttrValue, A] {
        override def apply(name: String): AttrCodec[F, A] =
          XmlCodec.attr[F](name) ~ codec
      }

  }

  object DeriveCodec extends DeriveCodecLow {

    private def collection[F[_]
    : Monad, C[_]
    : Traverse, D, X, A](cd: CardinalityDecoder[F, C, X, A])
                        (implicit ev: DeriveCodec[F, D, X, A]): DeriveCodec[F, D, C[X], C[A]] =
      new DeriveCodec[F, D, C[X], C[A]] {
        override def apply(name: String): XmlCodec[F, D, C[X], C[A]] =
          XmlCodec.collection(ev(name), cd)
      }

    implicit def option[F[_]:Monad, D, X, A](implicit ev: DeriveCodec[F, D, X, A]): DeriveCodec[F, D, Option[X], Option[A]] =
      collection[F, Option, D, X, A](CardinalityDecoder.option[F, X, A])

    implicit def list[F[_]:Monad, D, X, A](implicit ev: DeriveCodec[F, D, X, A]): DeriveCodec[F, D, List[X], List[A]] =
      collection[F, List, D, X, A](CardinalityDecoder.list[F, X, A])

    implicit def nel[F[_]:Monad, D, X, A](implicit ev: DeriveCodec[F, D, X, A]): DeriveCodec[F, D, NonEmptyList[X], NonEmptyList[A]] =
      collection[F, NonEmptyList, D, X, A](CardinalityDecoder.nel[F, X, A])

    implicit def attrInstanceS[F[_]:Monad, A]: DeriveCodec[F, String, String @@ AttrValue, String] =
      new DeriveCodec[F, String, String @@ AttrValue, String] {
        override def apply(name: String): AttrCodec[F, String] =
          XmlCodec.attr(name)
      }

    object ToName extends Poly1 {
      implicit def keyToName[A]: ToName.Case[Symbol with A] { type Result = String } = at[Symbol with A](_.name)
    }

    implicit def elemInstance[F[_]:Monad, A, G <: HList, LG <: HList, K <: HList, KS <: HList, CS <: HList, CR <: HList]
    (implicit
     classTag: ClassTag[A],
     generic: Generic.Aux[A, G],
     labelledGeneric: LabelledGeneric.Aux[A, LG],
     keys: Keys.Aux[LG, K],
     mapper: Mapper.Aux[ToName.type, K, KS],
     mkDerive: MkDerive[F, G, KS, CS],
     reverse: Reverse.Aux[CS, CR],
     HListDecoder: HListDecoder[F, CS, G],
     HListEncoder: HListEncoder[F, CS, G]): DeriveCodec[F, String, Elem, A] =
      new DeriveCodec[F, String, Elem, A] {
        val _ = (generic, labelledGeneric)
        override def apply(name: String): ElemCodec[F, A] = {
          val children = mkDerive(keys().map(ToName))
          XmlCodec.elem[F, CS, G](safeSimpleName(classTag.runtimeClass), children) ~
            Codec.fromFunctions(generic.from, generic.to)
        }
      }

    // Workaround for https://issues.scala-lang.org/browse/SI-2034
    // See https://github.com/json4s/json4s/commit/93c4fec6ff59a9e75f1fa2f87b36282e39b022ae
    private def safeSimpleName(clazz: Class[_]) =
      \/.fromTryCatchThrowable[String, Throwable](clazz.getSimpleName).getOrElse {
        val packageNameLen = Some(clazz.getPackage).map(_.getName.length + 1).getOrElse(0)
        stripDollar(clazz.getName.substring(packageNameLen))
      }

    private def stripDollar(name: String): String =
      name.split('$').toList
        .filterNot(_.isEmpty)
        .filterNot(_.matches("\\d+"))
        .lastOption
        .getOrElse(name)

  }

  trait MkDerive[F[_], G <: HList, K <: HList, O <: HList] {
    def apply(keys: K): O
  }

  object MkDerive {

    implicit def hNilDerive[F[_]]: MkDerive[F, HNil, HNil, HNil] =
      new MkDerive[F, HNil, HNil, HNil] {
        override def apply(keys: HNil): HNil = HNil
      }

    implicit def hConsDerive[F[_]:Monad, D, X, A, T <: HList, TK <: HList, TO <: HList
    ](implicit
      headDerive: DeriveCodec[F, D, X, A],
      tailDerive: MkDerive[F, T, TK, TO]): MkDerive[F, A :: T, String :: TK, XmlCodec[F, D, X, A] :: TO] =
      new MkDerive[F, A :: T, String :: TK, XmlCodec[F, D, X, A] :: TO] {
        override def apply(keys: String :: TK): XmlCodec[F, D, X, A] :: TO = {
          val key :: tailKeys = keys
          headDerive.apply(key) :: tailDerive(tailKeys)
        }
      }

  }

  implicit def instance[F[_]:Monad, A
  ](implicit deriveCodec: DeriveCodec[F, String, Elem, A]): Derive[F, A] =
    new Derive[F, A] {
      override def apply: ElemCodec[F, A] =
        deriveCodec.apply("")
    }

}
