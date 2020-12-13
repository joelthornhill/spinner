package spinner.util

import cats.MonadError
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
      result <- M.delay(f(addr, scale))
    } yield result

  def runner[F[_]](
    lfo: Lfo,
    freq: Freq,
    amp: Amp,
    f: (Int, Int, Int) => Unit
  )(implicit consts: Consts, M: Sync[F]): F[Unit] =
    for {
      lfo <- getInt(lfo.value)
      freq <- getInt(freq.value)
      amp <- getInt(amp.value)
      result <- M.delay(f(lfo, freq, amp))
    } yield result

  def runner[F[_]](
    scale: Scale,
    offset: Offset,
    f: (Double, Double) => Unit
  )(implicit consts: Consts, M: Sync[F]) =
    for {
      scale <- getDouble(scale.value)
      offset <- getDouble(offset.value)
      result <- M.delay(f(scale, offset))
    } yield result

  def runner[F[_]](
    lfo: Lfo,
    flags: Flags,
    offset: Offset,
    f: (Int, Int, Double) => Unit
  )(implicit consts: Consts, M: Sync[F]) =
    for {
      lfo <- getInt(lfo.value)
      flags <- getInt(flags.value)
      offset <- getDouble(offset.value)
      result <- M.delay(f(lfo, flags, offset))
    } yield result

  def findInReserved[F[_]](s: String)(implicit M: MonadError[F, Throwable]): F[Double] =
    M.catchNonFatal(ReservedWord.withName(s.toUpperCase).value.toDouble)

  def findInConsts[F[_]](
    s: String
  )(implicit M: MonadError[F, Throwable], consts: Consts): F[Double] = {

    def handleNotFound(value: String): F[Double] =
      consts.get(value) match {
        case Some(StringValue(stringValue)) =>
          findInReserved(stringValue).handleErrorWith(_ =>
            M.raiseError(SpinError(s"Could not find: $stringValue in consts"))
          )
        case Some(DoubleValue(doubleValue)) => M.pure(doubleValue)
        case Some(a: Arithmetic)            => a.run
        case _                              => M.raiseError(SpinError(s"Could not find $value in consts"))
      }

    consts.get(s) match {
      case Some(StringValue(value)) =>
        findInReserved(value).handleErrorWith(_ => handleNotFound(value))
      case Some(DoubleValue(value))     => M.pure(value)
      case Some(arithmetic: Arithmetic) => arithmetic.run
      case None                         => M.raiseError(SpinError(s"Could not find: $s in consts"))
    }
  }

  def getInt[F[_]](
    addr: InstructionValue
  )(implicit consts: Consts, M: MonadError[F, Throwable]): F[Int] =
    getDouble(addr).flatMap(d => M.catchNonFatal(d.toInt))

  def getDouble[F[_]](
    addr: InstructionValue
  )(implicit consts: Consts, M: MonadError[F, Throwable]): F[Double] =
    addr match {
      case DoubleValue(value) => M.pure(value)
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
  )(implicit M: MonadError[F, Throwable], consts: Consts) =
    for {
      scale <- getDouble(scale.value)
      run <- addr.value match {
        case DelayEnd(StringValue(value)) =>
          M.pure(f(value, 1.0, scale))
        case MidpointDelay(value) =>
          M.pure(f(value.value, 0.5, scale))
        case Addition(StringValue(value), DoubleValue(offset)) =>
          M.pure(f2(value, offset.toInt, scale))
        case StringValue(value) =>
          M.pure(f(value, 0.0, scale))
        case _ => getInt(addr.value).map(f3(_, scale))
      }
    } yield run
}
