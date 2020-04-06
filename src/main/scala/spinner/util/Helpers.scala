package spinner.util

import cats.effect.Sync
import spinner.model._
import spinner.Instruction
import spinner.ReservedWord
import spinner.Spin
import cats.syntax.functor._
import cats.syntax.flatMap._
import cats.syntax.applicativeError._

object Helpers {

  def updateProgram[F[_]: Sync](
    spin: Spin,
    instruction: Instruction
  ): F[(Spin, Unit)] = {
    val newInstructions =
      Spin.copy(spin.memoryMap, spin.instList, spin.consts)
    instruction.run(newInstructions).map((newInstructions, _))
  }

  def runnerID[F[_]: Sync](
    addr: InstructionValue,
    scale: InstructionValue,
    f: (Int, Double) => Unit
  )(consts: Map[String, InstructionValue]): F[Unit] = {
    for {
      addr <- getInt(addr)(consts)
      scale <- getDouble(scale, consts)
      run <- Sync[F].delay(f(addr, scale))
    } yield run
  }

  def runner[F[_]: Sync](
    lfo: InstructionValue,
    freq: InstructionValue,
    amp: InstructionValue,
    f: (Int, Int, Int) => Unit
  )(consts: Map[String, InstructionValue]) = {
    for {
      lfo <- getInt(lfo)(consts)
      freq <- getInt(freq)(consts)
      amp <- getInt(amp)(consts)
      run <- Sync[F].delay(f(lfo, freq, amp))
    } yield run
  }

  def runner[F[_]: Sync](
    value: InstructionValue,
    f: Int => Unit
  )(consts: Map[String, InstructionValue]) = {
    for {
      value <- getInt(value)(consts)
      run <- Sync[F].delay(f(value))
    } yield run
  }

  def runnerDD[F[_]: Sync](
    scale: InstructionValue,
    offset: InstructionValue,
    f: (Double, Double) => Unit
  )(consts: Map[String, InstructionValue]) =
    for {
      scale <- getDouble(scale, consts)
      offset <- getDouble(offset, consts)
      run <- Sync[F].delay(f(scale, offset))
    } yield run

  def runnerIID[F[_]: Sync](
    lfo: InstructionValue,
    flags: InstructionValue,
    offset: InstructionValue,
    f: (Int, Int, Double) => Unit
  )(consts: Map[String, InstructionValue]) =
    for {
      lfo <- getInt(lfo)(consts)
      flags <- getInt(flags)(consts)
      offset <- getDouble(offset, consts)
      run <- Sync[F].delay(f(lfo, flags, offset))
    } yield run

  def findInReserved[F[_]: Sync](s: String): F[Double] =
    Sync[F].catchNonFatal(ReservedWord.withName(s.toUpperCase).value.toDouble)

  def findInConsts[F[_]: Sync](
    s: String,
    getDouble: InstructionValue => F[Double]
  )(consts: Map[String, InstructionValue]): F[Double] = {
    Sync[F].pure(consts.get(s)).flatMap {
      case Some(StringValue(value)) =>
        findInReserved(value).attempt.flatMap {
          case Right(const) => Sync[F].pure(const)
          case Left(_) =>
            Sync[F].pure(consts.get(value)).flatMap {
              case Some(StringValue(stringValue)) =>
                findInReserved(stringValue).attempt flatMap {
                  case Right(v) => Sync[F].pure(v)
                  case _ =>
                    Sync[F].raiseError(new Exception(s"Could not find: $stringValue in consts"))
                }
              case Some(DoubleValue(doubleValue)) => Sync[F].pure(doubleValue)
              case Some(WithArithmetic(withArithmetic)) =>
                calculateArithmetic(withArithmetic, getDouble)
              case _ => Sync[F].raiseError(new Exception(s"Could not find $value in consts"))
            }
        }
      case Some(DoubleValue(value))    => Sync[F].pure(value)
      case Some(WithArithmetic(value)) => calculateArithmetic(value, getDouble)
      case None                        => Sync[F].raiseError(new Exception(s"Could not find: $s in consts"))
    }
  }

  def getInt[F[_]: Sync](
    addr: InstructionValue
  )(consts: Map[String, InstructionValue]): F[Int] =
    getDouble(addr, consts).flatMap(d => Sync[F].catchNonFatal(d.toInt))

  def getDouble[F[_]: Sync](
    addr: InstructionValue,
    consts: Map[String, InstructionValue]
  ): F[Double] =
    addr match {
      case DoubleValue(value) => Sync[F].pure(value)
      case StringValue(value) =>
        findInReserved[F](value).handleErrorWith(_ =>
          findInConsts(value, getDouble(_, consts))(consts)
        )
      case WithArithmetic(value) => calculateArithmetic(value, getDouble(_, consts))
    }

  def handleAllOffsets[F[_]: Sync](
    addr: InstructionValue,
    scale: InstructionValue,
    f: (String, Double, Double) => Unit,
    f2: (String, Int, Double) => Unit,
    f3: (Int, Double) => Unit
  )(consts: Map[String, InstructionValue]) =
    for {
      scale <- getDouble(scale, consts)
      run <- addr match {
        case WithArithmetic(DelayEnd(StringValue(value))) =>
          Sync[F].delay(f(value, 1.0, scale))
        case WithArithmetic(MidpointDelay(StringValue(value))) =>
          Sync[F].delay(f(value, 0.5, scale))
        case WithArithmetic(Addition(StringValue(value), DoubleValue(offset))) =>
          Sync[F].delay(f2(value, offset.toInt, scale))
        case StringValue(value) =>
          Sync[F].delay(f(value, 0.0, scale))
        case _ => runnerID(addr, DoubleValue(scale), f3)(consts)
      }
    } yield run

  def calculateArithmetic[F[_]: Sync](
    arithmetic: Arithmetic,
    getDouble: InstructionValue => F[Double]
  ): F[Double] = {
    arithmetic match {
      case Division(a, b) =>
        for {
          a <- getDouble(a)
          b <- getDouble(b)
        } yield a / b
      case Addition(a, b) =>
        for {
          a <- getDouble(a)
          b <- getDouble(b)
        } yield a + b
      case Minus(value) => getDouble(value).map(-_)
      case Multiplication(a, b) =>
        for {
          a <- getDouble(a)
          b <- getDouble(b)
        } yield a * b
      case DelayEnd(_) => Sync[F].raiseError(new Exception("Delay end should be handled elsewhere"))
      case MidpointDelay(_) =>
        Sync[F].raiseError(new Exception("Midpoint should be handled elsewhere"))
      case Or(value) =>
        value.foldLeft(Sync[F].pure(0.0)) {
          case (acc, b) =>
            for {
              left <- acc
              right <- getDouble(b)
              asDouble <- getDouble(DoubleValue((left.toInt | right.toInt).toDouble))
            } yield asDouble
        }
    }
  }
}
