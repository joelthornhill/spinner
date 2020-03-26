package example
import cats.effect.Sync
import example.Instruction.Instruction
import example.ParserCombinator._
import org.andrewkilpatrick.elmGen.ElmProgram
import cats.syntax.flatMap._
import cats.syntax.functor._

class Instructions[F[_]: Sync](consts: Map[String, InstructionValue]) extends ElmProgram("Parser") {

  def getInt(addr: InstructionValue): F[Int] =
    getDouble(addr).flatMap(d => Sync[F].catchNonFatal(d.toInt))

  def getDouble(addr: InstructionValue): F[Double] = {
    addr match {
      case Reserved(reservedWord) => Sync[F].pure(reservedWord.value.toDouble)
      case DoubleValue(value)     => Sync[F].pure(value)
      case StringValue(value)     => findInConsts(value)
      case WithArithmetic(value)  => calculateArithmetic(value)
    }
  }

  def findInConsts(s: String): F[Double] = {
    Sync[F].pure(consts.get(s)).flatMap {
      case Some(StringValue(value)) =>
        Sync[F].catchNonFatal(ReservedWord.withName(value.toUpperCase).value.toDouble)
      case Some(DoubleValue(value))    => Sync[F].pure(value)
      case Some(WithArithmetic(value)) => calculateArithmetic(value)
      case _                           => Sync[F].raiseError(new Exception(s"Could not find: $s"))
    }
  }

  def calculateArithmetic(arithmetic: Arithmetic): F[Double] = {
    arithmetic match {
      case Division(a, b) =>
        for {
          a <- getDouble(a)
          b <- getDouble(b)
        } yield a / b
      case Minus(a) => getDouble(a).map(-_)
      case Multiplication(a, b) =>
        for {
          a <- getDouble(a)
          b <- getDouble(b)
        } yield a * b
      case DelayEnd(a)            => getDouble(a) // Fix
      case MidpointDelay(a)       => getDouble(a) // Fix
      case ParserCombinator.Or(a) => getDouble(a.head) // Fix
    }
  }

  case class Rdax(addr: InstructionValue, scale: InstructionValue) extends Instruction[F] {
    def run(): F[Unit] =
      for {
        addr <- getInt(addr)
        scale <- getDouble(scale)
        run <- Sync[F].delay(readRegister(addr, scale))
      } yield run

    override def toString = s"readRegister(${addr.toString}, $scale)"
  }

  case class Rda(memName: String, offset: Double, scale: InstructionValue) extends Instruction[F] {
    def run() = getDouble(scale).flatMap(scale => Sync[F].delay(readDelay(memName, offset, scale)))

    override def toString: String = s"readDelay($memName, $offset, $scale)"
  }

  case class Wrax(addr: InstructionValue, scale: InstructionValue) extends Instruction[F] {
    def run() =
      for {
        addr <- getInt(addr)
        scale <- getDouble(scale)
        run <- Sync[F].delay(writeRegister(addr, scale))
      } yield run

    override def toString = s"writeRegister($addr, $scale)"
  }

  case class Wrap(memName: String, offset: Double, scale: InstructionValue) extends Instruction[F] {
    def run() =
      getDouble(scale).flatMap(scale => Sync[F].delay(writeAllpass(memName, offset, scale)))
    def runString() = s"writeAllpass($memName, $offset, ${getDouble(scale)})"

    override def toString: String = s"writeAllpass($memName, $offset, $scale)"
  }

  case class Wra(memName: String, offset: Double, scale: InstructionValue) extends Instruction[F] {
    def run() = getDouble(scale).flatMap(scale => Sync[F].delay(writeDelay(memName, offset, scale)))

    override def toString: String = s"writeDelay($memName, $offset, $scale)"
  }

  case class Sof(scale: InstructionValue, offset: InstructionValue) extends Instruction[F] {
    def run() =
      for {
        scale <- getDouble(scale)
        offset <- getDouble(offset)
        run <- Sync[F].delay(scaleOffset(scale, offset))
      } yield run

    override def toString: String = s"scaleOffset($scale, $offset)"
  }

  case class Equ(name: String, value: String) extends Instruction[F] {
    def run() = Sync[F].unit // Do nothing
    override def toString = ""
  }

  case class Mem(name: String, value: InstructionValue) extends Instruction[F] {
    def run() = getInt(value).flatMap(size => Sync[F].delay(allocDelayMem(name, size)))

    override def toString = s"allocDelayMem($name, $value)"
  }

  case object EOF extends Instruction[F] {
    def run() = Sync[F].unit
  }

  case class Skp(flags: InstructionValue, nSkip: InstructionValue) extends Instruction[F] {
    def run() =
      for {
        flags <- getInt(flags)
        nSkip <- getInt(nSkip)
        run <- Sync[F].delay(skip(flags, nSkip))
      } yield run

    override def toString: String = s"skip($flags, $nSkip)"
  }

  case object Clr extends Instruction[F] {
    def run() = Sync[F].delay(clear())

    override def toString: String = "clear()"
  }

  case class Exp(scale: InstructionValue, offset: InstructionValue) extends Instruction[F] {
    def run() =
      for {
        scale <- getDouble(scale)
        offset <- getDouble(offset)
        run <- Sync[F].delay(exp(scale, offset))
      } yield run

    override def toString: String = s"exp($scale, $offset)"
  }

  case class Mulx(addr: InstructionValue) extends Instruction[F] {
    def run() = getInt(addr).flatMap(a => Sync[F].delay(mulx(a)))

    override def toString: String = s"mulx($addr)"
  }

  case class Wldr(lfo: InstructionValue, freq: InstructionValue, amp: InstructionValue)
      extends Instruction[F] {
    def run() =
      for {
        lfo <- getInt(lfo)
        freq <- getInt(freq)
        amp <- getInt(amp)
        run <- Sync[F].delay(loadRampLFO(lfo, freq, amp))
      } yield run

    override def toString: String = s"loadRampLFO($lfo, $freq, $amp)"
  }

  case class ChoRda(lfo: InstructionValue, flags: InstructionValue, addr: InstructionValue)
      extends Instruction[F] {
    def run() =
      for {
        lfo <- getInt(lfo)
        flags <- getInt(flags)
        addr <- getInt(addr)
        run <- Sync[F].delay(chorusReadDelay(lfo, flags, addr))
      } yield run

    override def toString: String = s"chorusReadDelay($lfo, $flags, $addr)"
  }

  case class ChoSof(lfo: InstructionValue, flags: InstructionValue, offset: InstructionValue)
      extends Instruction[F] {
    def run() =
      for {
        lfo <- getInt(lfo)
        flags <- getInt(flags)
        offset <- getDouble(offset)
        run <- Sync[F].delay(chorusScaleOffset(lfo, flags, offset))
      } yield run

    override def toString: String = s"chorusReadDelay($lfo, $flags, $offset)"
  }

  case class ChoRdal(lfo: InstructionValue) extends Instruction[F] {
    def run() = getInt(lfo).flatMap(lfo => Sync[F].delay(chorusReadValue(lfo)))

    override def toString: String = s"chorusReadValue($lfo)"
  }

  case class Wlds(lfo: InstructionValue, freq: InstructionValue, amp: InstructionValue)
      extends Instruction[F] {
    def run() =
      for {
        lfo <- getInt(lfo)
        freq <- getDouble(freq)
        amp <- getDouble(amp)
        run <- Sync[F].delay(loadSinLFO(lfo, freq, amp))
      } yield run

    override def toString: String = s"loadSinLFO($lfo, $freq, $amp)"
  }

  case class Rdfx(addr: InstructionValue, scale: InstructionValue) extends Instruction[F] {

    def run() =
      for {
        addr <- getInt(addr)
        scale <- getDouble(scale)
        run <- Sync[F].delay(readRegisterFilter(addr, scale))
      } yield run

    override def toString: String = s"readRegisterFilter($addr, $scale)"

  }

  case class Rmpa(scale: InstructionValue) extends Instruction[F] {
    def run() = getDouble(scale).flatMap(scale => Sync[F].delay(readDelayPointer(scale)))

    override def toString: String = s"readDelayPointer($scale)"
  }

  case class Wrlx(addr: InstructionValue, scale: InstructionValue) extends Instruction[F] {
    def run() =
      for {
        addr <- getInt(addr)
        scale <- getDouble(scale)
        run <- Sync[F].delay(writeRegisterLowshelf(addr, scale))
      } yield run

    override def toString: String = s"writeRegisterLowshelf($addr, $scale)"
  }

  case class Wrhx(addr: InstructionValue, scale: InstructionValue) extends Instruction[F] {
    def run() =
      for {
        addr <- getInt(addr)
        scale <- getDouble(scale)
        run <- Sync[F].delay(writeRegisterHighshelf(addr, scale))
      } yield run

    override def toString: String = s"writeRegisterHighshelf($addr, $scale)"
  }

  case class Log(scale: InstructionValue, offset: InstructionValue) extends Instruction[F] {
    def run() =
      for {
        scale <- getDouble(scale)
        offset <- getDouble(offset)
        run <- Sync[F].delay(log(scale, offset))
      } yield run

    override def toString: String = s"log($scale, $offset)"
  }

  case class And(mask: InstructionValue) extends Instruction[F] {
    def run() = getInt(mask).flatMap(mask => Sync[F].delay(and(mask)))

    override def toString: String = s"and($mask)"
  }

  case class Or(mask: InstructionValue) extends Instruction[F] {
    def run() = getInt(mask).flatMap(mask => Sync[F].delay(or(mask)))

    override def toString: String = s"or($mask)"
  }

  case object Loop extends Instruction[F] {
    def run() = Sync[F].unit
  }
}

object Instruction {
  sealed trait Instruction[F[_]] {
    def run(): F[Unit]
  }
}
