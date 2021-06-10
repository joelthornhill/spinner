package spinner

import cats.effect.Sync
import spinner.Instruction.Consts
import spinner.model.InstructionValue
import spinner.util.Helpers.getDouble
import spinner.util.Helpers.getInt
import cats.syntax.functor._

object Params {
  sealed trait ParamType {
    val value: InstructionValue
    def spinString: String = value.spinString
  }
  case class Addr(value: InstructionValue) extends ParamType {
    def run[F[_]: Sync](f: Int => Unit)(implicit c: Consts): F[Unit] =
      getInt(value).map(f(_))
  }

  case class Scale(value: InstructionValue) extends ParamType {
    def run[F[_]: Sync](f: Double => Unit)(implicit c: Consts): F[Unit] =
      getDouble(value).map(f(_))
  }
  case class Offset(value: InstructionValue) extends ParamType

  case class Lfo(value: InstructionValue) extends ParamType {
    def run[F[_]: Sync](f: Int => Unit)(implicit c: Consts): F[Unit] =
      getInt(value).map(f(_))
  }

  case class Freq(value: InstructionValue) extends ParamType
  case class Amp(value: InstructionValue) extends ParamType
  case class Flags(value: InstructionValue) extends ParamType

  case class Mask(value: InstructionValue) extends ParamType {
    def run[F[_]: Sync](f: Int => Unit)(implicit c: Consts): F[Unit] =
      getInt(value).map(f(_))
  }

  case class EquValue(value: InstructionValue) extends ParamType {
    def run[F[_]: Sync](f: Int => Unit)(implicit c: Consts): F[Unit] =
      getInt(value).map(f(_))
  }
  case class NSkip(value: InstructionValue) extends ParamType

}
