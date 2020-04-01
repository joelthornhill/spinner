package spinner
import cats.effect.Sync
import spinner.Instruction.Instruction
import spinner.ParserCombinator._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.applicativeError._
import org.andrewkilpatrick.elmGen.SpinProgram
import Instruction._

class Instructions[F[_]: Sync](consts: Map[String, InstructionValue])
    extends SpinProgram("Parser") {

  setSamplerate(44100)

  def getInt(addr: InstructionValue): F[Int] =
    getDouble(addr).flatMap(d => Sync[F].catchNonFatal(d.toInt))

  def getDouble(addr: InstructionValue): F[Double] = {
    addr match {
      case DoubleValue(value) => Sync[F].pure(value)
      case StringValue(value) =>
        findInReserved(value).handleErrorWith(_ => findInConsts(value, consts, getDouble))
      case WithArithmetic(value) => calculateArithmetic(value, getDouble)
    }
  }

  def handleAllOffsets(
    addr: InstructionValue,
    scale: InstructionValue,
    f: (String, Double, Double) => Unit,
    f2: (String, Int, Double) => Unit,
    f3: (Int, Double) => Unit
  ) =
    for {
      scale <- getDouble(scale)
      run <- addr match {
        case WithArithmetic(DelayEnd(StringValue(value))) =>
          Sync[F].delay(f(value, 1.0, scale))
        case WithArithmetic(MidpointDelay(StringValue(value))) =>
          Sync[F].delay(f(value, 0.5, scale))
        case WithArithmetic(Addition(StringValue(value), DoubleValue(offset))) =>
          Sync[F].delay(f2(value, offset.toInt, scale))
        case StringValue(value) =>
          Sync[F].delay(f(value, 0.0, scale))
        case _ => runnerID(addr, DoubleValue(scale), f3)
      }
    } yield run

  case class Rdax(addr: InstructionValue, scale: InstructionValue) extends Instruction[F] {
    def run(): F[Unit] = runnerID(addr, scale, readRegister)
    def runString(): F[Unit] = runnerID(addr, scale, (i, d) => println(s"readRegister($i, $d)"))
    override def toString = s"readRegister(${addr.toString}, $scale)"
    override def spinInstruction(): String = s"rdax ${addr.spinString},${scale.spinString}"
  }

  case class Rda(addr: InstructionValue, scale: InstructionValue) extends Instruction[F] {
    def run(): F[Unit] = handleAllOffsets(
      addr,
      scale,
      readDelay(_: String, _: Double, _: Double),
      readDelay(_: String, _: Int, _: Double),
      readDelay(_: Int, _: Double)
    )

    def runString(): F[Unit] = handleAllOffsets(
      addr,
      scale,
      (v, d1, d2) => println(s"""readDelay(\"$v\", $d1, $d2)"""),
      (v, i, d) => println(s"""readDelay(\"$v\", $i, $d)"""),
      (i, d) => println(s"readDelay($i, $d)")
    )

    override def toString: String = s"readDelay($addr, $scale)"

    override def spinInstruction(): String = s"rda ${addr.spinString}, ${scale.spinString}"
  }

  case class Wrax(addr: InstructionValue, scale: InstructionValue) extends Instruction[F] {
    def run(): F[Unit] = runnerID(addr, scale, writeRegister)
    def runString(): F[Unit] =
      runnerID(addr, scale, (i, d) => println(s"writeRegister($i, $d)"))
    override def toString = s"writeRegister($addr, $scale)"
    override def spinInstruction(): String = s"wrax ${addr.spinString}, ${scale.spinString}"
  }

  case class Wrap(addr: InstructionValue, scale: InstructionValue) extends Instruction[F] {
    def run(): F[Unit] = handleAllOffsets(
      addr,
      scale,
      (v, d1, d2) => writeAllpass(v, d1, d2),
      (v, i, d) => writeAllpass(v, i, d),
      (i, d) => writeAllpass(i, d)
    )

    def runString(): F[Unit] = handleAllOffsets(
      addr,
      scale,
      (v, d1, d2) => println(s"""writeAllpass(\"$v\", $d1, $d2)"""),
      (v, i, d) => println(s"""writeAllpass(\"$v\", $i, $d)"""),
      (i, d) => println(s"writeAllpass($i, $d)")
    )
    override def toString: String = s"writeAllpass($addr, $scale)"

    override def spinInstruction(): String = s"wrap ${addr.spinString}, ${scale.spinString}"
  }

  case class Wra(addr: InstructionValue, scale: InstructionValue) extends Instruction[F] {
    def run(): F[Unit] = handleAllOffsets(
      addr,
      scale,
      (v, d1, d2) => writeDelay(v, d1, d2),
      (v, i, d) => writeDelay(v, i, d),
      (i, d) => writeDelay(i, d)
    )

    def runString(): F[Unit] = handleAllOffsets(
      addr,
      scale,
      (v, d1, d2) => println(s"""writeDelay(\"$v\", $d1, $d2)"""),
      (v, i, d) => println(s"""writeDelay(\"$v\", $i, $d)"""),
      (i, d) => println(s"writeDelay($i, $d)")
    )

    override def toString: String = s"writeDelay($addr, $scale)"

    override def spinInstruction(): String = s"wra ${addr.spinString}, ${scale.spinString}"
  }

  case class Sof(scale: InstructionValue, offset: InstructionValue) extends Instruction[F] {
    def run(): F[Unit] = runnerDD(scale, offset, scaleOffset)
    def runString(): F[Unit] =
      runnerDD(scale, offset, (d1, d2) => println(s"scaleOffset($d1, $d2)"))
    override def toString: String = s"scaleOffset($scale, $offset)"
    override def spinInstruction(): String = s"sof ${scale.spinString},${offset.spinString}"
  }

  case class Equ(name: String, value: InstructionValue) extends Instruction[F] {
    def run() = Sync[F].unit // Do nothing
    def runString() =
      Sync[F].delay(println(s"val $name = ${value.spinString.toUpperCase}")) // Do nothing
    override def toString = ""

    override def spinInstruction(): String = s"equ $name ${value.spinString}"
  }

  case class Mem(name: String, value: InstructionValue) extends Instruction[F] {
    def run() = getInt(value).flatMap(size => Sync[F].delay(allocDelayMem(name, size)))
    def runString() =
      getInt(value).flatMap(size => Sync[F].delay(println(s"""allocDelayMem("$name", $size)""")))

    override def toString = s"allocDelayMem($name, $value)"

    override def spinInstruction(): String = s"mem $name  ${value.spinString}"
  }

  case object EOF extends Instruction[F] {
    def run() = Sync[F].unit
    def runString() = Sync[F].unit

    override def spinInstruction(): String = ""
  }

  case class Skp(flags: InstructionValue, nSkip: InstructionValue) extends Instruction[F] {
    def run() =
      for {
        flags <- getInt(flags)
        run <- nSkip match {
          case StringValue(_) => Sync[F].unit
          case _              => getInt(nSkip).flatMap(nSkip => Sync[F].delay(skip(flags, nSkip)))
        }
      } yield run

    def runString() =
      for {
        flags <- getInt(flags)
        nSkip <- getInt(nSkip)
        run <- Sync[F].delay(println(s"skip($flags, $nSkip)"))
      } yield run

    override def toString: String = s"skip($flags, $nSkip)"

    override def spinInstruction(): String = s"skip ${flags.spinString},${nSkip.spinString}"
  }

//  case class Skp2(flags: InstructionValue, point: String) extends Instruction[F] {
//    def run() = Sync[F].unit // Do nothing
//    def runString() = Sync[F].delay(println("Skp2 does nothing"))
//    def replace(nSkip: InstructionValue) = Skp(flags, nSkip)
//
//    override def toString: String = s"skip($flags, $point)"
//
//    override def spinInstruction(): String = s"skp ${flags.spinString},$point"
//  }

  case class SkipLabel(label: String) extends Instruction[F] {
    def run(): F[Unit] = Sync[F].unit
    def runString(): F[Unit] = Sync[F].delay(println("Skip Label does nothing"))

    override def spinInstruction(): String = s"Skip label: $label"
  }

  case object Clr extends Instruction[F] {
    def run(): F[Unit] = Sync[F].delay(clear())
    def runString(): F[Unit] = Sync[F].delay(println("clear()"))
    override def toString: String = "clear()"
    override def spinInstruction(): String = "clr"
  }

  case class Exp(scale: InstructionValue, offset: InstructionValue) extends Instruction[F] {
    def run(): F[Unit] = runnerDD(scale, offset, exp)
    def runString(): F[Unit] = runnerDD(scale, offset, (d1, d2) => println(s"exp($d1, $d2)"))
    override def toString: String = s"exp($scale, $offset)"
    override def spinInstruction(): String = s"exp ${scale.spinString},${offset.spinString}"
  }

  case class Mulx(addr: InstructionValue) extends Instruction[F] {
    def run(): F[Unit] = runner(addr, mulx)
    def runString(): F[Unit] = runner(addr, i => println(s"mulx($i)"))
    override def toString: String = s"mulx($addr)"
    override def spinInstruction(): String = s"mulx ${addr.spinString}"
  }

  case class Wldr(lfo: InstructionValue, freq: InstructionValue, amp: InstructionValue)
      extends Instruction[F] {
    def run(): F[Unit] = runner(lfo, freq, amp, loadRampLFO)
    def runString(): F[Unit] =
      runner(lfo, freq, amp, (i1, i2, i3) => println(s"loadRampLFO($i1, $i2, $i3)"))
    override def toString: String = s"loadRampLFO($lfo, $freq, $amp)"
    override def spinInstruction(): String =
      s"wldr ${lfo.spinString},${freq.spinString},${amp.spinString}"
  }

  case class ChoRda(lfo: InstructionValue, flags: InstructionValue, addr: InstructionValue)
      extends Instruction[F] {
    def run() =
      for {
        lfo <- getInt(lfo)
        flags <- getInt(flags)
        run <- addr match {
          case WithArithmetic(Addition(StringValue(value), DoubleValue(offset))) =>
            Sync[F].delay(chorusReadDelay(lfo, flags, value, offset.toInt))
          case _ => getInt(addr).flatMap(addr => Sync[F].delay(chorusReadDelay(lfo, flags, addr)))
        }
      } yield run

    def runString() =
      for {
        lfo <- getInt(lfo)
        flags <- getInt(flags)
        run <- addr match {
          case WithArithmetic(Addition(StringValue(value), DoubleValue(offset))) =>
            Sync[F].delay(println(s"chorusReadDelay($lfo, $flags, $value, ${offset.toInt})"))
          case _ =>
            getInt(addr).flatMap(addr =>
              Sync[F].delay(println(s"chorusReadDelay($lfo, $flags, $addr)"))
            )
        }
      } yield run

    override def toString: String = s"chorusReadDelay($lfo, $flags, $addr)"

    override def spinInstruction(): String =
      s"cho rda, ${lfo.spinString},${flags.spinString},${addr.spinString}"
  }

  case class ChoSof(lfo: InstructionValue, flags: InstructionValue, offset: InstructionValue)
      extends Instruction[F] {
    def run(): F[Unit] = runnerIID(lfo, flags, offset, chorusScaleOffset)
    def runString(): F[Unit] =
      runnerIID(lfo, flags, offset, (i1, i2, d1) => println(s"chorusScaleOffset($i1, $i2, $d1)"))
    override def toString: String = s"chorusScaleOffset($lfo, $flags, $offset)"
    override def spinInstruction(): String =
      s"cho sof,${lfo.spinString},${flags.spinString},${offset.spinString}"
  }

  case class ChoRdal(lfo: InstructionValue) extends Instruction[F] {
    def run(): F[Unit] = runner(lfo, chorusReadValue)
    def runString(): F[Unit] = runner(lfo, i => println(s"chorusReadValue($i)"))
    override def toString: String = s"chorusReadValue($lfo)"
    override def spinInstruction(): String = s"cho rdal,${lfo.spinString}"
  }

  case class Wlds(lfo: InstructionValue, freq: InstructionValue, amp: InstructionValue)
      extends Instruction[F] {
    def run(): F[Unit] = runner(lfo, freq, amp, loadSinLFO)
    def runString(): F[Unit] =
      runner(lfo, freq, amp, (i1, i2, i3) => println(s"loadSinLFO($lfo, $freq, $amp)"))
    override def toString: String = s"loadSinLFO($lfo, $freq, $amp)"
    override def spinInstruction(): String =
      s"wlds ${lfo.spinString},${freq.spinString},${amp.spinString}"
  }

  case class Rdfx(addr: InstructionValue, scale: InstructionValue) extends Instruction[F] {
    def run(): F[Unit] = runnerID(addr, scale, readRegisterFilter)
    def runString(): F[Unit] =
      runnerID(addr, scale, (i, d) => println(s"readRegisterFilter($i, $d)"))
    override def toString: String = s"readRegisterFilter($addr, $scale)"
    override def spinInstruction(): String = s"rdfx ${addr.spinString}, ${scale.spinString}"
  }

  case class Rmpa(scale: InstructionValue) extends Instruction[F] {
    def run() = getDouble(scale).flatMap(scale => Sync[F].delay(readDelayPointer(scale)))
    def runString() =
      getDouble(scale).flatMap(scale => Sync[F].delay(println(s"readDelayPointer($scale)")))

    override def toString: String = s"readDelayPointer($scale)"

    override def spinInstruction(): String = s"rmpa ${scale.spinString}"
  }

  case class Wrlx(addr: InstructionValue, scale: InstructionValue) extends Instruction[F] {
    def run(): F[Unit] = runnerID(addr, scale, writeRegisterLowshelf)
    def runString(): F[Unit] =
      runnerID(addr, scale, (i, d) => println(s"writeRegisterLowshelf($i, $d)"))
    override def toString: String = s"writeRegisterLowshelf($addr, $scale)"
    override def spinInstruction(): String = s"wrlx ${addr.spinString},${scale.spinString}"
  }

  case class Wrhx(addr: InstructionValue, scale: InstructionValue) extends Instruction[F] {
    def run(): F[Unit] = runnerID(addr, scale, writeRegisterHighshelf)
    def runString(): F[Unit] =
      runnerID(addr, scale, (i, d) => println(s"writeRegisterHighshelf($i, $d)"))
    override def toString: String = s"writeRegisterHighshelf($addr, $scale)"
    override def spinInstruction(): String = s"wrhx ${addr.spinString}, ${scale.spinString}"
  }

  case class Log(scale: InstructionValue, offset: InstructionValue) extends Instruction[F] {
    def run(): F[Unit] = runnerDD(scale, offset, log)
    def runString(): F[Unit] = runnerDD(scale, offset, (d1, d2) => println(s"log($d1, $d2)"))
    override def toString: String = s"log($scale, $offset)"
    override def spinInstruction(): String = s"log ${scale.spinString},${offset.spinString}"
  }

  case class And(mask: InstructionValue) extends Instruction[F] {
    def run(): F[Unit] = runner(mask, and)
    def runString(): F[Unit] = runner(mask, i => println(s"and($i)"))
    override def toString: String = s"and($mask)"
    override def spinInstruction(): String = s"and ${mask.spinString}"
  }

  case class Or(mask: InstructionValue) extends Instruction[F] {
    def run(): F[Unit] = runner(mask, or)
    def runString(): F[Unit] = runner(mask, i => println(s"or($i)"))
    override def toString: String = s"or($mask)"
    override def spinInstruction(): String = s"or ${mask.spinString}"
  }

//  case object Loop extends Instruction[F] {
//    def run() = Sync[F].unit
//    def runString() = Sync[F].delay(println("loop does nothing"))
//
//    override def spinInstruction(): String = s"loop"
//  }

  private def runnerID(
    addr: InstructionValue,
    scale: InstructionValue,
    f: (Int, Double) => Unit
  ) = {
    for {
      addr <- getInt(addr)
      scale <- getDouble(scale)
      run <- Sync[F].delay(f(addr, scale))
    } yield run
  }

  private def runner(
    lfo: InstructionValue,
    freq: InstructionValue,
    amp: InstructionValue,
    f: (Int, Int, Int) => Unit
  ) = {
    for {
      lfo <- getInt(lfo)
      freq <- getInt(freq)
      amp <- getInt(amp)
      run <- Sync[F].delay(f(lfo, freq, amp))
    } yield run
  }

  private def runner(
    value: InstructionValue,
    f: Int => Unit
  ) = {
    for {
      value <- getInt(value)
      run <- Sync[F].delay(f(value))
    } yield run
  }

  private def runnerDD(
    scale: InstructionValue,
    offset: InstructionValue,
    f: (Double, Double) => Unit
  ) =
    for {
      scale <- getDouble(scale)
      offset <- getDouble(offset)
      run <- Sync[F].delay(f(scale, offset))
    } yield run

  private def runnerIID(
    lfo: InstructionValue,
    flags: InstructionValue,
    offset: InstructionValue,
    f: (Int, Int, Double) => Unit
  ) =
    for {
      lfo <- getInt(lfo)
      flags <- getInt(flags)
      offset <- getDouble(offset)
      run <- Sync[F].delay(f(lfo, flags, offset))
    } yield run
}

object Instruction {
  sealed trait Instruction[F[_]] {
    def run(): F[Unit]
    def runString(): F[Unit]
    def spinInstruction(): String
  }

  def findInReserved[F[_]: Sync](s: String): F[Double] =
    Sync[F].catchNonFatal(ReservedWord.withName(s.toUpperCase).value.toDouble)

  def findInConsts[F[_]: Sync](
    s: String,
    consts: Map[String, InstructionValue],
    getDouble: InstructionValue => F[Double]
  ): F[Double] = {
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
      case ParserCombinator.Or(value) =>
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
