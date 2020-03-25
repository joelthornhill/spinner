package example
import cats.effect.Sync
import example.EquParser.EquDoubleValue
import example.EquParser.EquIntegerValue
import example.EquParser.EquStringValue
import example.EquParser.EquValue
import example.Instruction.Instruction
import example.ParserCombinator._
import org.andrewkilpatrick.elmGen.ElmProgram
import cats.syntax.flatMap._
import cats.syntax.functor._

class Instructions[F[_]: Sync](consts: Map[String, EquValue]) extends ElmProgram("Parser") {

  def getInt(addr: MapsToInteger): F[Int] = {
    addr match {
      case reserved: Reserved         => Sync[F].pure(reserved.reservedWord.value)
      case integerValue: IntegerValue => Sync[F].pure(integerValue.value)
      case stringValue: StringValue =>
        getReservedInt(stringValue.value).fold[F[Int]](
          Sync[F].raiseError(new Exception(s"Could not find: $stringValue"))
        )(Sync[F].pure)
    }
  }

  def getReservedInt(s: String): Option[Int] = {
    consts.get(s).flatMap {
      case equString: EquStringValue =>
        Some(ReservedWord.withName(equString.value.toUpperCase).value)
      case equInteger: EquIntegerValue =>
        Some(equInteger.value)
      case _ => None
    }
  }

  def getReservedDouble(s: String): Option[Double] = {
    consts.get(s).flatMap {
      case equDoubleValue: EquDoubleValue   => Some(equDoubleValue.value)
      case equIntegerValue: EquIntegerValue => Some(equIntegerValue.value.toDouble)
      case _                                => None
    }
  }

  def getDouble(addr: MapsToDouble): F[Double] = {
    val doubleValue = addr match {
      case d: DoubleValue => Some(d.value)
      case s: StringValue => getReservedDouble(s.value)
      case _              => None
    }

    doubleValue.fold[F[Double]](Sync[F].raiseError(new Exception(s"Could not find: $addr")))(
      Sync[F].pure
    )
  }

  case class Rdax(addr: MapsToInteger, scale: MapsToDouble) extends Instruction[F] {
    def run(): F[Unit] =
      for {
        addr <- getInt(addr)
        scale <- getDouble(scale)
        run <- Sync[F].delay(readRegister(addr, scale))
      } yield run

    override def toString = s"readRegister(${addr.toString}, $scale)"
  }

  case class Rda(memName: String, offset: Double, scale: MapsToDouble) extends Instruction[F] {
    def run() = getDouble(scale).flatMap(scale => Sync[F].delay(readDelay(memName, offset, scale)))

    override def toString: String = s"readDelay($memName, $offset, $scale)"
  }

  case class Wrax(addr: MapsToInteger, scale: MapsToDouble) extends Instruction[F] {
    def run() =
      for {
        addr <- getInt(addr)
        scale <- getDouble(scale)
        run <- Sync[F].delay(writeRegister(addr, scale))
      } yield run

    override def toString = s"writeRegister($addr, $scale)"
  }

  case class Wrap(memName: String, offset: Double, scale: MapsToDouble) extends Instruction[F] {
    def run() =
      getDouble(scale).flatMap(scale => Sync[F].delay(writeAllpass(memName, offset, scale)))
    def runString() = s"writeAllpass($memName, $offset, ${getDouble(scale)})"

    override def toString: String = s"writeAllpass($memName, $offset, $scale)"
  }

  case class Wra(memName: String, offset: Double, scale: MapsToDouble) extends Instruction[F] {
    def run() = getDouble(scale).flatMap(scale => Sync[F].delay(writeDelay(memName, offset, scale)))

    override def toString: String = s"writeDelay($memName, $offset, $scale)"
  }

  case class Sof(scale: MapsToDouble, offset: MapsToDouble) extends Instruction[F] {
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

  case class Mem(name: String, value: MapsToInteger) extends Instruction[F] {
    def run() = getInt(value).flatMap(size => Sync[F].delay(allocDelayMem(name, size)))

    override def toString = s"allocDelayMem($name, $value)"
  }

  case object EOF extends Instruction[F] {
    def run() = Sync[F].unit
  }

  case class Skp(flags: MapsToInteger, nSkip: MapsToInteger) extends Instruction[F] {
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

  case class Exp(scale: MapsToDouble, offset: MapsToDouble) extends Instruction[F] {
    def run() =
      for {
        scale <- getDouble(scale)
        offset <- getDouble(offset)
        run <- Sync[F].delay(exp(scale, offset))
      } yield run

    override def toString: String = s"exp($scale, $offset)"
  }

  case class Mulx(addr: MapsToInteger) extends Instruction[F] {
    def run() = getInt(addr).flatMap(a => Sync[F].delay(mulx(a)))

    override def toString: String = s"mulx($addr)"
  }

  case class Wldr(lfo: MapsToInteger, freq: MapsToInteger, amp: MapsToInteger)
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

  case class ChoRda(lfo: MapsToInteger, flags: MapsToInteger, addr: MapsToInteger)
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

  case class ChoSof(lfo: MapsToInteger, flags: MapsToInteger, offset: MapsToDouble)
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

  case class ChoRdal(lfo: MapsToInteger) extends Instruction[F] {
    def run() = getInt(lfo).flatMap(lfo => Sync[F].delay(chorusReadValue(lfo)))

    override def toString: String = s"chorusReadValue($lfo)"
  }

  case class Wlds(lfo: MapsToInteger, freq: MapsToDouble, amp: MapsToDouble)
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

  case class Rdfx(addr: MapsToInteger, scale: MapsToDouble) extends Instruction[F] {

    def run() =
      for {
        addr <- getInt(addr)
        scale <- getDouble(scale)
        run <- Sync[F].delay(readRegisterFilter(addr, scale))
      } yield run

    override def toString: String = s"readRegisterFilter($addr, $scale)"

  }

  case class Rmpa(scale: MapsToDouble) extends Instruction[F] {
    def run() = getDouble(scale).flatMap(scale => Sync[F].delay(readDelayPointer(scale)))

    override def toString: String = s"readDelayPointer($scale)"
  }

  case class Wrlx(addr: MapsToInteger, scale: MapsToDouble) extends Instruction[F] {
    def run() =
      for {
        addr <- getInt(addr)
        scale <- getDouble(scale)
        run <- Sync[F].delay(writeRegisterLowshelf(addr, scale))
      } yield run

    override def toString: String = s"writeRegisterLowshelf($addr, $scale)"
  }

  case class Wrhx(addr: MapsToInteger, scale: MapsToDouble) extends Instruction[F] {
    def run() =
      for {
        addr <- getInt(addr)
        scale <- getDouble(scale)
        run <- Sync[F].delay(writeRegisterHighshelf(addr, scale))
      } yield run

    override def toString: String = s"writeRegisterHighshelf($addr, $scale)"
  }

  case class Log(scale: MapsToDouble, offset: MapsToDouble) extends Instruction[F] {
    def run() =
      for {
        scale <- getDouble(scale)
        offset <- getDouble(offset)
        run <- Sync[F].delay(log(scale, offset))
      } yield run

    override def toString: String = s"log($scale, $offset)"
  }

  case class And(mask: MapsToInteger) extends Instruction[F] {
    def run() = getInt(mask).flatMap(mask => Sync[F].delay(and(mask)))

    override def toString: String = s"and($mask)"
  }

  case class Or(mask: MapsToInteger) extends Instruction[F] {
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
