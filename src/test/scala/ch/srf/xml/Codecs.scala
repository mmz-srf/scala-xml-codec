package ch.srf.xml

import java.time.LocalDate

import scalaz.syntax.tag._
import scalaz.{@@, Monad, Tag}

object Codecs {

  implicit def localDateCodec[F[_]:Monad]: Codec[F, String, LocalDate] =
    Codec.from(
      Decoder.fromTryCatchNonFatal(LocalDate.parse),
      Encoder.fromFunction(_.toString)
    )

  implicit def tagCodec[F[_]:Monad, A, T]: Codec[F, A, A @@ T] =
    Codec.fromFunctions(Tag.of[T](_), _.unwrap)

}
