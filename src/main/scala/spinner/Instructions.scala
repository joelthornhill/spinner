package spinner
import java.util
import org.andrewkilpatrick.elmGen.MemSegment
import org.andrewkilpatrick.elmGen.SpinProgram

import cats.effect.Sync
import spinner.ParserCombinator._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.applicativeError._

case class Instructions(consts: Map[String, InstructionValue]) extends SpinProgram("Hello") {
  setSamplerate(44100)
}

object Instructions {

  def copy(
    memoryMap: util.LinkedList[MemSegment],
    instList: util.LinkedList[org.andrewkilpatrick.elmGen.instructions.Instruction],
    consts: Map[String, InstructionValue]
  ): Instructions = {
    val newInstructions = new Instructions(consts)
    newInstructions.setMemoryMap(memoryMap)
    newInstructions.setInstList(instList)
    newInstructions
  }

}

object Instruction {

  def updateProgram[F[_]: Sync](
    instructions: Instructions,
    instruction: Instruction
  ): F[(Instructions, Unit)] = {
    val newInstructions =
      Instructions.copy(instructions.memoryMap, instructions.instList, instructions.consts)
    instruction.run(newInstructions).map((newInstructions, _))
  }

  sealed trait Instruction {
    def run[F[_]: Sync](instructions: Instructions): F[Unit]
    def runString[F[_]: Sync](instructions: Instructions): F[Unit]
    def spinInstruction(): String = ""
  }

  case class Rdax(addr: InstructionValue, scale: InstructionValue) extends Instruction {
    def run[F[_]: Sync](instructions: Instructions): F[Unit] =
      runnerID(addr, scale, instructions.readRegister)(instructions.consts)
    def runString[F[_]: Sync](instructions: Instructions): F[Unit] =
      runnerID(addr, scale, (i, d) => println(s"readRegister($i, $d)"))(instructions.consts)
    override def toString = s"readRegister(${addr.toString}, $scale)"
    override def spinInstruction(): String = s"rdax ${addr.spinString},${scale.spinString}"
  }

  case class Rda(addr: InstructionValue, scale: InstructionValue) extends Instruction {
    def run[F[_]: Sync](
      instructions: Instructions
    ): F[Unit] =
      handleAllOffsets(
        addr,
        scale,
        instructions.readDelay(_: String, _: Double, _: Double),
        instructions.readDelay(_: String, _: Int, _: Double),
        instructions.readDelay(_: Int, _: Double)
      )(instructions.consts)

    def runString[F[_]: Sync](instructions: Instructions): F[Unit] =
      handleAllOffsets(
        addr,
        scale,
        (v, d1, d2) => println(s"""readDelay(\"$v\", $d1, $d2)"""),
        (v, i, d) => println(s"""readDelay(\"$v\", $i, $d)"""),
        (i, d) => println(s"readDelay($i, $d)")
      )(instructions.consts)

    override def toString: String = s"readDelay($addr, $scale)"
    override def spinInstruction(): String = s"rda ${addr.spinString}, ${scale.spinString}"
  }

  case class Wrax(addr: InstructionValue, scale: InstructionValue) extends Instruction {
    def run[F[_]: Sync](instructions: Instructions): F[Unit] =
      runnerID(addr, scale, instructions.writeRegister)(instructions.consts)
    def runString[F[_]: Sync](instructions: Instructions): F[Unit] =
      runnerID(addr, scale, (i, d) => println(s"writeRegister($i, $d)"))(instructions.consts)
    override def toString = s"writeRegister($addr, $scale)"
    override def spinInstruction(): String = s"wrax ${addr.spinString}, ${scale.spinString}"
  }

  case class Maxx(addr: InstructionValue, scale: InstructionValue) extends Instruction {
    def run[F[_]: Sync](instructions: Instructions): F[Unit] =
      runnerID(addr, scale, instructions.maxx)(instructions.consts)
    def runString[F[_]: Sync](instructions: Instructions): F[Unit] =
      runnerID(addr, scale, (i, d) => println(s"maxx($i, $d)"))(instructions.consts)
    override def toString = s"maxx($addr, $scale)"
    override def spinInstruction(): String = s"maxx ${addr.spinString}, ${scale.spinString}"
  }

  case class Wrap(addr: InstructionValue, scale: InstructionValue) extends Instruction {
    def run[F[_]: Sync](
      instructions: Instructions
    ): F[Unit] =
      handleAllOffsets(
        addr,
        scale,
        (v, d1, d2) => instructions.writeAllpass(v, d1, d2),
        (v, i, d) => instructions.writeAllpass(v, i, d),
        (i, d) => instructions.writeAllpass(i, d)
      )(instructions.consts)

    def runString[F[_]: Sync](instructions: Instructions): F[Unit] =
      handleAllOffsets(
        addr,
        scale,
        (v, d1, d2) => println(s"""writeAllpass(\"$v\", $d1, $d2)"""),
        (v, i, d) => println(s"""writeAllpass(\"$v\", $i, $d)"""),
        (i, d) => println(s"writeAllpass($i, $d)")
      )(instructions.consts)
    override def toString: String = s"writeAllpass($addr, $scale)"
    override def spinInstruction(): String = s"wrap ${addr.spinString}, ${scale.spinString}"
  }

  case class Wra(addr: InstructionValue, scale: InstructionValue) extends Instruction {
    def run[F[_]: Sync](
      instructions: Instructions
    ): F[Unit] =
      handleAllOffsets(
        addr,
        scale,
        (v, d1, d2) => instructions.writeDelay(v, d1, d2),
        (v, i, d) => instructions.writeDelay(v, i, d),
        (i, d) => instructions.writeDelay(i, d)
      )(instructions.consts)

    def runString[F[_]: Sync](instructions: Instructions): F[Unit] =
      handleAllOffsets(
        addr,
        scale,
        (v, d1, d2) => println(s"""writeDelay(\"$v\", $d1, $d2)"""),
        (v, i, d) => println(s"""writeDelay(\"$v\", $i, $d)"""),
        (i, d) => println(s"writeDelay($i, $d)")
      )(instructions.consts)

    override def toString: String = s"writeDelay($addr, $scale)"
    override def spinInstruction(): String = s"wra ${addr.spinString}, ${scale.spinString}"
  }

  case class Sof(scale: InstructionValue, offset: InstructionValue) extends Instruction {
    def run[F[_]: Sync](instructions: Instructions): F[Unit] =
      runnerDD(scale, offset, instructions.scaleOffset)(instructions.consts)
    def runString[F[_]: Sync](instructions: Instructions): F[Unit] =
      runnerDD(scale, offset, (d1, d2) => println(s"scaleOffset($d1, $d2)"))(instructions.consts)
    override def toString: String = s"scaleOffset($scale, $offset)"
    override def spinInstruction(): String = s"sof ${scale.spinString},${offset.spinString}"
  }

  case class Equ(name: String, value: InstructionValue) extends Instruction {
    def run[F[_]: Sync](instructions: Instructions): F[Unit] = Sync[F].unit // Do nothing
    def runString[F[_]: Sync](instructions: Instructions): F[Unit] =
      Sync[F].delay(println(s"val $name = ${value.spinString.toUpperCase}")) // Do nothing
    override def toString = ""
    override def spinInstruction(): String = s"equ $name ${value.spinString}"
  }

  case class Mem(name: String, value: InstructionValue) extends Instruction {
    def run[F[_]: Sync](
      instructions: Instructions
    ): F[Unit] =
      getInt(value)(instructions.consts).flatMap(size =>
        Sync[F].delay(instructions.allocDelayMem(name, size))
      )
    def runString[F[_]: Sync](instructions: Instructions) =
      getInt(value)(instructions.consts).flatMap(size =>
        Sync[F].delay(println(s"""allocDelayMem("$name", $size)"""))
      )
    override def toString = s"allocDelayMem($name, $value)"
    override def spinInstruction(): String = s"mem $name  ${value.spinString}"
  }

  case object EOF extends Instruction {
    def run[F[_]: Sync](instructions: Instructions): F[Unit] = Sync[F].unit
    def runString[F[_]: Sync](instructions: Instructions) = Sync[F].unit
    override def spinInstruction(): String = ""
  }

  case class Skp(flags: InstructionValue, nSkip: InstructionValue) extends Instruction {
    def run[F[_]: Sync](
      instructions: Instructions
    ): F[Unit] =
      for {
        flags <- getInt(flags)(instructions.consts)
        run <- nSkip match {
          case StringValue(_) => Sync[F].unit
          case _ =>
            getInt(nSkip)(instructions.consts).flatMap(nSkip =>
              Sync[F].delay(instructions.skip(flags, nSkip))
            )
        }
      } yield run

    def runString[F[_]: Sync](instructions: Instructions) =
      for {
        flags <- getInt(flags)(instructions.consts)
        nSkip <- getInt(nSkip)(instructions.consts)
        run <- Sync[F].delay(println(s"skip($flags, $nSkip)"))
      } yield run

    override def toString: String = s"skp($flags, $nSkip)"
    override def spinInstruction(): String = s"skp ${flags.spinString},${nSkip.spinString}"
  }
//
//  //  case class Skp2(flags: InstructionValue, point: String) extends Instruction[F] {
//  //    def run() = Sync[F].unit // Do nothing
//  //    def runString() = Sync[F].delay(println("Skp2 does nothing"))
//  //    def replace(nSkip: InstructionValue) = Skp(flags, nSkip)
//  //
//  //    override def toString: String = s"skip($flags, $point)"
//  //
//  //    override def spinInstruction(): String = s"skp ${flags.spinString},$point"
//  //  }
//
  case class SkipLabel(label: String) extends Instruction {
    def run[F[_]: Sync](instructions: Instructions): F[Unit] = Sync[F].unit
    def runString[F[_]: Sync](instructions: Instructions): F[Unit] =
      Sync[F].delay(println("Skip Label does nothing"))

    override def spinInstruction(): String = s"$label:"
  }

  case object Clr extends Instruction {
    def run[F[_]: Sync](instructions: Instructions): F[Unit] = Sync[F].delay(instructions.clear())
    def runString[F[_]: Sync](instructions: Instructions): F[Unit] =
      Sync[F].delay(println("clear()"))
    override def toString: String = "clear()"
    override def spinInstruction(): String = "clr"
  }

  case object Absa extends Instruction {
    def run[F[_]: Sync](instructions: Instructions): F[Unit] = Sync[F].delay(instructions.absa())
    def runString[F[_]: Sync](instructions: Instructions): F[Unit] =
      Sync[F].delay(println("absa()"))
    override def toString: String = "absa()"
    override def spinInstruction(): String = "absa"
  }

  case class Exp(scale: InstructionValue, offset: InstructionValue) extends Instruction {
    def run[F[_]: Sync](instructions: Instructions): F[Unit] =
      runnerDD(scale, offset, instructions.exp)(instructions.consts)
    def runString[F[_]: Sync](instructions: Instructions): F[Unit] =
      runnerDD(scale, offset, (d1, d2) => println(s"exp($d1, $d2)"))(instructions.consts)
    override def toString: String = s"exp($scale, $offset)"
    override def spinInstruction(): String = s"exp ${scale.spinString},${offset.spinString}"
  }

  case class Mulx(addr: InstructionValue) extends Instruction {
    def run[F[_]: Sync](instructions: Instructions): F[Unit] =
      runner(addr, instructions.mulx)(instructions.consts)
    def runString[F[_]: Sync](instructions: Instructions): F[Unit] =
      runner(addr, i => println(s"mulx($i)"))(instructions.consts)
    override def toString: String = s"mulx($addr)"
    override def spinInstruction(): String = s"mulx ${addr.spinString}"
  }

  case class Xor(mask: InstructionValue) extends Instruction {
    def run[F[_]: Sync](instructions: Instructions): F[Unit] =
      runner(mask, instructions.xor)(instructions.consts)
    def runString[F[_]: Sync](instructions: Instructions): F[Unit] =
      runner(mask, i => println(s"xor($i)"))(instructions.consts)
    override def toString: String = s"xor($mask)"
    override def spinInstruction(): String = s"xor ${mask.spinString}"
  }

  case class Wldr(lfo: InstructionValue, freq: InstructionValue, amp: InstructionValue)
      extends Instruction {
    def run[F[_]: Sync](instructions: Instructions): F[Unit] =
      runner(lfo, freq, amp, instructions.loadRampLFO)(instructions.consts)
    def runString[F[_]: Sync](instructions: Instructions): F[Unit] =
      runner(lfo, freq, amp, (i1, i2, i3) => println(s"loadRampLFO($i1, $i2, $i3)"))(
        instructions.consts
      )
    override def toString: String = s"loadRampLFO($lfo, $freq, $amp)"
    override def spinInstruction(): String =
      s"wldr ${lfo.spinString},${freq.spinString},${amp.spinString}"
  }

  case class ChoRda(lfo: InstructionValue, flags: InstructionValue, addr: InstructionValue)
      extends Instruction {
    def run[F[_]: Sync](
      instructions: Instructions
    ) =
      for {
        lfo <- getInt(lfo)(instructions.consts)
        flags <- getInt(flags)(instructions.consts)
        run <- addr match {
          case WithArithmetic(Addition(StringValue(value), DoubleValue(offset))) =>
            Sync[F].delay(instructions.chorusReadDelay(lfo, flags, value, offset.toInt))
          case _ =>
            getInt(addr)(instructions.consts).flatMap(addr =>
              Sync[F].delay(instructions.chorusReadDelay(lfo, flags, addr))
            )
        }
      } yield run

    def runString[F[_]: Sync](instructions: Instructions) =
      for {
        lfo <- getInt(lfo)(instructions.consts)
        flags <- getInt(flags)(instructions.consts)
        run <- addr match {
          case WithArithmetic(Addition(StringValue(value), DoubleValue(offset))) =>
            Sync[F].delay(println(s"chorusReadDelay($lfo, $flags, $value, ${offset.toInt})"))
          case _ =>
            getInt(addr)(instructions.consts).flatMap(addr =>
              Sync[F].delay(println(s"chorusReadDelay($lfo, $flags, $addr)"))
            )
        }
      } yield run

    override def toString: String = s"chorusReadDelay($lfo, $flags, $addr)"

    override def spinInstruction(): String =
      s"cho rda, ${lfo.spinString},${flags.spinString},${addr.spinString}"
  }

  case class ChoSof(lfo: InstructionValue, flags: InstructionValue, offset: InstructionValue)
      extends Instruction {
    def run[F[_]: Sync](instructions: Instructions): F[Unit] =
      runnerIID(lfo, flags, offset, instructions.chorusScaleOffset)(instructions.consts)
    def runString[F[_]: Sync](instructions: Instructions): F[Unit] =
      runnerIID(lfo, flags, offset, (i1, i2, d1) => println(s"chorusScaleOffset($i1, $i2, $d1)"))(
        instructions.consts
      )
    override def toString: String = s"chorusScaleOffset($lfo, $flags, $offset)"
    override def spinInstruction(): String =
      s"cho sof,${lfo.spinString},${flags.spinString},${offset.spinString}"
  }

  case class ChoRdal(lfo: InstructionValue) extends Instruction {
    def run[F[_]: Sync](instructions: Instructions): F[Unit] =
      runner(lfo, instructions.chorusReadValue)(instructions.consts)
    def runString[F[_]: Sync](instructions: Instructions): F[Unit] =
      runner(lfo, i => println(s"chorusReadValue($i)"))(instructions.consts)
    override def toString: String = s"chorusReadValue($lfo)"
    override def spinInstruction(): String = s"cho rdal,${lfo.spinString}"
  }

  case class Wlds(lfo: InstructionValue, freq: InstructionValue, amp: InstructionValue)
      extends Instruction {
    def run[F[_]: Sync](instructions: Instructions): F[Unit] =
      runner(lfo, freq, amp, instructions.loadSinLFO)(instructions.consts)
    def runString[F[_]: Sync](instructions: Instructions): F[Unit] =
      runner(lfo, freq, amp, (i1, i2, i3) => println(s"loadSinLFO($lfo, $freq, $amp)"))(
        instructions.consts
      )
    override def toString: String = s"loadSinLFO($lfo, $freq, $amp)"
    override def spinInstruction(): String =
      s"wlds ${lfo.spinString},${freq.spinString},${amp.spinString}"
  }

  case class Rdfx(addr: InstructionValue, scale: InstructionValue) extends Instruction {
    def run[F[_]: Sync](instructions: Instructions): F[Unit] =
      runnerID(addr, scale, instructions.readRegisterFilter)(instructions.consts)
    def runString[F[_]: Sync](instructions: Instructions): F[Unit] =
      runnerID(addr, scale, (i, d) => println(s"readRegisterFilter($i, $d)"))(instructions.consts)
    override def toString: String = s"readRegisterFilter($addr, $scale)"
    override def spinInstruction(): String = s"rdfx ${addr.spinString}, ${scale.spinString}"
  }

  case class Rmpa(scale: InstructionValue) extends Instruction {
    def run[F[_]: Sync](instructions: Instructions) =
      getDouble(scale, instructions.consts).flatMap(scale =>
        Sync[F].delay(instructions.readDelayPointer(scale))
      )
    def runString[F[_]: Sync](instructions: Instructions): F[Unit] =
      getDouble(scale, instructions.consts).flatMap(scale =>
        Sync[F].delay(println(s"readDelayPointer($scale)"))
      )

    override def toString: String = s"readDelayPointer($scale)"
    override def spinInstruction(): String = s"rmpa ${scale.spinString}"
  }

  case class Wrlx(addr: InstructionValue, scale: InstructionValue) extends Instruction {
    def run[F[_]: Sync](instructions: Instructions): F[Unit] =
      runnerID(addr, scale, instructions.writeRegisterLowshelf)(instructions.consts)
    def runString[F[_]: Sync](instructions: Instructions): F[Unit] =
      runnerID(addr, scale, (i, d) => println(s"writeRegisterLowshelf($i, $d)"))(
        instructions.consts
      )
    override def toString: String = s"writeRegisterLowshelf($addr, $scale)"
    override def spinInstruction(): String = s"wrlx ${addr.spinString},${scale.spinString}"
  }

  case class Wrhx(addr: InstructionValue, scale: InstructionValue) extends Instruction {
    def run[F[_]: Sync](instructions: Instructions): F[Unit] =
      runnerID(addr, scale, instructions.writeRegisterHighshelf)(instructions.consts)
    def runString[F[_]: Sync](instructions: Instructions): F[Unit] =
      runnerID(addr, scale, (i, d) => println(s"writeRegisterHighshelf($i, $d)"))(
        instructions.consts
      )
    override def toString: String = s"writeRegisterHighshelf($addr, $scale)"
    override def spinInstruction(): String = s"wrhx ${addr.spinString}, ${scale.spinString}"
  }

  case class Log(scale: InstructionValue, offset: InstructionValue) extends Instruction {
    def run[F[_]: Sync](instructions: Instructions): F[Unit] =
      runnerDD(scale, offset, instructions.log)(instructions.consts)
    def runString[F[_]: Sync](instructions: Instructions): F[Unit] =
      runnerDD(scale, offset, (d1, d2) => println(s"log($d1, $d2)"))(instructions.consts)
    override def toString: String = s"log($scale, $offset)"
    override def spinInstruction(): String = s"log ${scale.spinString},${offset.spinString}"
  }
//
  case class And(mask: InstructionValue) extends Instruction {
    def run[F[_]: Sync](instructions: Instructions): F[Unit] =
      runner(mask, instructions.and)(instructions.consts)
    def runString[F[_]: Sync](instructions: Instructions): F[Unit] =
      runner(mask, i => println(s"and($i)"))(instructions.consts)
    override def toString: String = s"and($mask)"
    override def spinInstruction(): String = s"and ${mask.spinString}"
  }

  case class Or(mask: InstructionValue) extends Instruction {
    def run[F[_]: Sync](instructions: Instructions): F[Unit] =
      runner(mask, instructions.or)(instructions.consts)
    def runString[F[_]: Sync](instructions: Instructions): F[Unit] =
      runner(mask, i => println(s"or($i)"))(instructions.consts)
    override def toString: String = s"or($mask)"
    override def spinInstruction(): String = s"or ${mask.spinString}"
  }

  case object Loop extends Instruction {
    def run[F[_]: Sync](instructions: Instructions) = Sync[F].unit
    def runString[F[_]: Sync](instructions: Instructions) =
      Sync[F].delay(println("loop does nothing"))

    override def spinInstruction(): String = s"loop"
  }

  private def runnerID[F[_]: Sync](
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

  private def runner[F[_]: Sync](
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

  private def runner[F[_]: Sync](
    value: InstructionValue,
    f: Int => Unit
  )(consts: Map[String, InstructionValue]) = {
    for {
      value <- getInt(value)(consts)
      run <- Sync[F].delay(f(value))
    } yield run
  }

  private def runnerDD[F[_]: Sync](
    scale: InstructionValue,
    offset: InstructionValue,
    f: (Double, Double) => Unit
  )(consts: Map[String, InstructionValue]) =
    for {
      scale <- getDouble(scale, consts)
      offset <- getDouble(offset, consts)
      run <- Sync[F].delay(f(scale, offset))
    } yield run

  private def runnerIID[F[_]: Sync](
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
                calculateArithmetic(withArithmetic, getDouble)(consts)
              case _ => Sync[F].raiseError(new Exception(s"Could not find $value in consts"))
            }
        }
      case Some(DoubleValue(value))    => Sync[F].pure(value)
      case Some(WithArithmetic(value)) => calculateArithmetic(value, getDouble)(consts)
      case None                        => Sync[F].raiseError(new Exception(s"Could not find: $s in consts"))
    }
  }

  private def getInt[F[_]: Sync](
    addr: InstructionValue
  )(consts: Map[String, InstructionValue]): F[Int] =
    getDouble(addr, consts).flatMap(d => Sync[F].catchNonFatal(d.toInt))

  private def getDouble[F[_]: Sync](
    addr: InstructionValue,
    consts: Map[String, InstructionValue]
  ): F[Double] =
    addr match {
      case DoubleValue(value) => Sync[F].pure(value)
      case StringValue(value) =>
        findInReserved[F](value).handleErrorWith(_ =>
          findInConsts(value, getDouble(_, consts))(consts)
        )
      case WithArithmetic(value) => calculateArithmetic(value, getDouble(_, consts))(consts)
    }

  private def handleAllOffsets[F[_]: Sync](
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
  )(consts: Map[String, InstructionValue]): F[Double] = {
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
