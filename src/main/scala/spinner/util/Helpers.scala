package spinner.util

import cats.data.StateT
import cats.effect.Sync
import spinner.model._
import spinner.Params._
import spinner.Instruction
import spinner.ReservedWord
import spinner.Spin
import cats.syntax.functor._
import cats.syntax.flatMap._
import cats.syntax.applicativeError._
import spinner.Instruction.Consts

object Helpers {

  case class SpinError(msg: String) extends Throwable

  def updateProgram[F[_]](
    instruction: Instruction[F]
  )(implicit M: Sync[F]): StateT[F, Spin, Unit] = StateT[F, Spin, Unit] { s =>
    val newInstructions = Spin.copy(s.memoryMap, s.instList, s.consts)
    implicit val consts: Consts = s.consts
    instruction.run(newInstructions).map((newInstructions, _))
  }

  def runner[F[_]](
    addr: Addr,
    scale: Scale,
    f: (Int, Double) => Unit
  )(implicit M: Sync[F], consts: Consts): F[Unit] =
    for {
      addr <- getInt(addr.value)
      scale <- getDouble(scale.value)
    } yield f(addr, scale)

  def runner[F[_]: Sync](
    lfo: Lfo,
    freq: Freq,
    amp: Amp,
    f: (Int, Int, Int) => Unit
  )(implicit consts: Consts): F[Unit] =
    for {
      lfo <- getInt(lfo.value)
      freq <- getInt(freq.value)
      amp <- getInt(amp.value)
    } yield f(lfo, freq, amp)

  def runner[F[_]: Sync](
    scale: Scale,
    offset: Offset,
    f: (Double, Double) => Unit
  )(implicit consts: Consts) =
    for {
      scale <- getDouble(scale.value)
      offset <- getDouble(offset.value)
    } yield f(scale, offset)

  def runner[F[_]: Sync](
    lfo: Lfo,
    flags: Flags,
    offset: Offset,
    f: (Int, Int, Double) => Unit
  )(implicit consts: Consts) =
    for {
      lfo <- getInt(lfo.value)
      flags <- getInt(flags.value)
      offset <- getDouble(offset.value)
    } yield f(lfo, flags, offset)

  def findInReserved[F[_]: Sync](s: String): F[Double] =
    Sync[F].catchNonFatal(ReservedWord.withName(s.toUpperCase).value.toDouble)

  def findInConsts[F[_]](s: String)(implicit M: Sync[F], consts: Consts): F[Double] = {
    consts.get(s) match {
      case Some(StringValue(value)) =>
        findInReserved(value).attempt.flatMap {
          case Right(const) => M.pure(const)
          case Left(_) =>
            consts.get(value) match {
              case Some(StringValue(stringValue)) =>
                findInReserved(stringValue).attempt flatMap {
                  case Right(v) => M.pure(v)
                  case _ =>
                    M.raiseError(SpinError(s"Could not find: $stringValue in consts"))
                }
              case Some(DoubleValue(doubleValue)) => M.pure(doubleValue)
              case Some(a: Arithmetic)            => a.run
              case _                              => M.raiseError(SpinError(s"Could not find $value in consts"))
            }
        }
      case Some(DoubleValue(value)) => M.pure(value)
      case Some(a: Arithmetic)      => a.run
      case None                     => M.raiseError(SpinError(s"Could not find: $s in consts"))
    }
  }

  def getInt[F[_]: Sync](
    addr: InstructionValue
  )(implicit consts: Consts): F[Int] =
    getDouble(addr).flatMap(d => Sync[F].catchNonFatal(d.toInt))

  def getDouble[F[_]: Sync](
    addr: InstructionValue
  )(implicit consts: Consts): F[Double] =
    addr match {
      case DoubleValue(value) => Sync[F].pure(value)
      case StringValue(value) =>
        findInReserved[F](value).handleErrorWith(_ => findInConsts(value))
      case a: Arithmetic => a.run
    }

  def handleAllOffsets[F[_]](
    addr: Addr,
    scale: Scale,
    f: (String, Double, Double) => Unit,
    f2: (String, Int, Double) => Unit,
    f3: (Int, Double) => Unit
  )(implicit M: Sync[F], consts: Consts) =
    for {
      scale <- getDouble(scale.value)
      run <- addr.value match {
        case DelayEnd(StringValue(value)) =>
          M.pure(f(value, 1.0, scale))
        case MidpointDelay(StringValue(value)) =>
          M.pure(f(value, 0.5, scale))
        case Addition(StringValue(value), DoubleValue(offset)) =>
          M.pure(f2(value, offset.toInt, scale))
        case StringValue(value) =>
          M.pure(f(value, 0.0, scale))
        case _ => getInt(addr.value).map(f3(_, scale))
      }
    } yield run
}
