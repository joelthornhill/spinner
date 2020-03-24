package example
import example.EquParser.{EquDoubleValue, EquStringValue, EquValue}
import example.Instruction.Instruction
import example.ParserCombinator._
import org.andrewkilpatrick.elmGen.ElmProgram

class Instructions extends ElmProgram("Parser") {

  def getInt(addr: MapsToInteger, map: Map[String, EquValue]): Int = addr match {
    case r: Reserved => r.reservedWord.value
    case i: IntegerValue => i.value
    case s: StringValue => map.get(s.value).map {
      case v: EquStringValue => ReservedWord.withName(v.s.toUpperCase).value
      case _ => ???
    }.getOrElse(0)
  }

  def getDouble(addr: MapsToDouble, map: Map[String, EquValue]): Double = addr match {
    case _: Reserved => ???
    case d: DoubleValue => d.value
    case s: StringValue => map.get(s.value).map {
      case v: EquDoubleValue => v.d
      case _ => ???
    }.getOrElse(0)
  }

  case class Rdax(addr: MapsToInteger, scale: MapsToDouble) extends Instruction {
    def run(map: Map[String, EquValue]) = readRegister(getInt(addr, map), getDouble(scale, map))

    override def toString = s"readRegister(${addr.toString}, $scale)"
  }

  case class Rda(addr: MapsToInteger, scale: MapsToDouble) extends Instruction {
    def run(map: Map[String, EquValue]) = readDelay(getInt(addr, map), getDouble(scale, map))

    override def toString: String = s"readDelay($addr, $scale)"
  }

  case class Wrax(addr: MapsToInteger, scale: MapsToDouble) extends Instruction {
    def run(map: Map[String, EquValue]) = writeRegister(getInt(addr, map), getDouble(scale, map))

    override def toString = s"writeRegister($addr, $scale)"
  }

  case class Wrap(addr: MapsToInteger, scale: MapsToDouble) extends Instruction {
    def run(map: Map[String, EquValue]) = writeAllpass(getInt(addr, map), getDouble(scale, map))

    override def toString: String = s"writeAllpass($addr, $scale)"
  }

  case class Wra(addr: MapsToInteger, scale: MapsToDouble) extends Instruction {
    def run(map: Map[String, EquValue]) = writeDelay(getInt(addr, map), getDouble(scale, map))

    override def toString: String = s"writeDelay($addr, $scale)"
  }

  case class Sof(scale: MapsToDouble, offset: MapsToDouble) extends Instruction {
    def run(map: Map[String, EquValue]) = scaleOffset(getDouble(scale, map), getDouble(offset, map))

    override def toString: String = s"scaleOffset($scale, $offset)"
  }

  case class Equ(name: String, value: String) extends Instruction {
    def run(map: Map[String, EquValue]) = () // Do nothing
    override def toString = ""
  }

  case class Mem(name: String, value: MapsToInteger) extends Instruction {
    def run(map: Map[String, EquValue]) = allocDelayMem(name, getInt(value, map))

    override def toString = s"allocDelayMem($name, $value)"
  }

  case object EOF extends Instruction {
    def run(map: Map[String, EquValue]) = ()
  }

  case class Skp(flags: MapsToInteger, nSkip: MapsToInteger) extends Instruction {
    def run(map: Map[String, EquValue]) = skip(getInt(flags, map), getInt(nSkip, map))

    override def toString: String = s"skip($flags, $nSkip)"
  }

  case object Clr extends Instruction {
    def run(map: Map[String, EquValue]) = clear()

    override def toString: String = "clear()"
  }

  case class Exp(scale: MapsToDouble, offset: MapsToDouble) extends Instruction {
    def run(map: Map[String, EquValue]) = exp(getDouble(scale, map), getDouble(offset, map))

    override def toString: String = s"exp($scale, $offset)"
  }

  case class Mulx(addr: MapsToInteger) extends Instruction {
    def run(map: Map[String, EquValue]) = mulx(getInt(addr, map))

    override def toString: String = s"mulx($addr)"
  }

  case class Wldr(lfo: MapsToInteger, freq: MapsToInteger, amp: MapsToInteger) extends Instruction {
    def run(map: Map[String, EquValue]) = loadRampLFO(getInt(lfo, map), getInt(freq, map), getInt(amp, map))

    override def toString: String = s"loadRampLFO($lfo, $freq, $amp)"
  }

  case class ChoRda(lfo: MapsToInteger, flags: MapsToInteger, addr: MapsToInteger) extends Instruction {
    def run(map: Map[String, EquValue]) = chorusReadDelay(getInt(lfo, map), getInt(flags, map), getInt(addr, map))

    override def toString: String = s"chorusReadDelay($lfo, $flags, $addr)"
  }

  case class ChoSfo(lfo: MapsToInteger, flags: MapsToInteger, offset: MapsToDouble) extends Instruction {
    def run(map: Map[String, EquValue]) = chorusScaleOffset(getInt(lfo, map), getInt(flags, map), getDouble(offset, map))

    override def toString: String = s"chorusReadDelay($lfo, $flags, $offset)"
  }

}

object Instruction {
  sealed trait Instruction {
    def run(map: Map[String, EquValue]): Unit
  }
}