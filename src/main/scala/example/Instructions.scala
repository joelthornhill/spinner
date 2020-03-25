package example
import example.EquParser.EquDoubleValue
import example.EquParser.EquIntegerValue
import example.EquParser.EquStringValue
import example.EquParser.EquValue
import example.Instruction.Instruction
import example.ParserCombinator._
import org.andrewkilpatrick.elmGen.ElmProgram

class Instructions extends ElmProgram("Parser") {

  def getInt(addr: MapsToInteger, map: Map[String, EquValue]): Int = {
    addr match {
      case r: Reserved     => r.reservedWord.value
      case i: IntegerValue => i.value
      case s: StringValue =>
        map
          .get(s.value.stripPrefix("-"))
          .map {
            case v: EquStringValue =>
              val result = ReservedWord.withName(v.s.toUpperCase).value
              if (s.value.contains("-")) -result else result
            case v: EquIntegerValue => if (s.value.contains("-")) -v.i else v.i
            case _                  => throw new Exception("Argggggg")
          }
          .getOrElse(throw new Exception(s"nonononono: $addr"))
    }
  }

  def getDouble(addr: MapsToDouble, map: Map[String, EquValue]): Double = addr match {
    case _: Reserved    => ???
    case d: DoubleValue => d.value
    case s: StringValue =>
      map
        .get(s.value.stripPrefix("-"))
        .map {
          case v: EquDoubleValue => if (s.value.contains("-")) -v.d else v.d
          case _                 => throw new Exception("Oh nooooo")
        }
        .getOrElse(throw new Exception(s"!!!!!!!!!: $addr"))
  }

  case class Rdax(addr: MapsToInteger, scale: MapsToDouble) extends Instruction {
    def run(map: Map[String, EquValue]) = readRegister(getInt(addr, map), getDouble(scale, map))
    def runString(map: Map[String, EquValue]) =
      s"readRegister(${getInt(addr, map)}, ${getDouble(scale, map)})"

    override def toString = s"readRegister(${addr.toString}, $scale)"
  }

  case class Rda(memName: String, offset: Double, scale: MapsToDouble) extends Instruction {
    def run(map: Map[String, EquValue]) = readDelay(memName, offset, getDouble(scale, map))
    def runString(map: Map[String, EquValue]) =
      s"readDelay($memName, $offset, ${getDouble(scale, map)})"

    override def toString: String = s"readDelay($memName, $offset, $scale)"
  }

  case class Wrax(addr: MapsToInteger, scale: MapsToDouble) extends Instruction {
    def run(map: Map[String, EquValue]) = writeRegister(getInt(addr, map), getDouble(scale, map))
    def runString(map: Map[String, EquValue]) =
      s"writeRegister(${getInt(addr, map)}, ${getDouble(scale, map)})"

    override def toString = s"writeRegister($addr, $scale)"
  }

  case class Wrap(memName: String, offset: Double, scale: MapsToDouble) extends Instruction {
//    def run(map: Map[String, EquValue]) = writeAllpass(getInt(addr, map), getDouble(scale, map))
    def run(map: Map[String, EquValue]) = writeAllpass(memName, offset, getDouble(scale, map))
    def runString(map: Map[String, EquValue]) =
      s"writeAllpass($memName, $offset, ${getDouble(scale, map)})"

    override def toString: String = s"writeAllpass($memName, $offset, $scale)"
  }

  case class Wra(memName: String, offset: Double, scale: MapsToDouble) extends Instruction {
//    def run(map: Map[String, EquValue]) = writeDelay(getInt(addr, map), getDouble(scale, map))
    def run(map: Map[String, EquValue]) = writeDelay(memName, offset, getDouble(scale, map))
    def runString(map: Map[String, EquValue]) =
      s"writeDelay($memName, $offset, ${getDouble(scale, map)})"

    override def toString: String = s"writeDelay($memName, $offset, $scale)"
  }

  case class Sof(scale: MapsToDouble, offset: MapsToDouble) extends Instruction {
    def run(map: Map[String, EquValue]) = scaleOffset(getDouble(scale, map), getDouble(offset, map))
    def runString(map: Map[String, EquValue]) =
      s"scaleOffset(${getDouble(scale, map)}, ${getDouble(offset, map)})"

    override def toString: String = s"scaleOffset($scale, $offset)"
  }

  case class Equ(name: String, value: String) extends Instruction {
    def run(map: Map[String, EquValue]) = () // Do nothing
    def runString(map: Map[String, EquValue]) = "" // Do nothing
    override def toString = ""
  }

  case class Mem(name: String, value: MapsToInteger) extends Instruction {
    def run(map: Map[String, EquValue]) = allocDelayMem(name, getInt(value, map))
    def runString(map: Map[String, EquValue]) = s"allocDelayMem($name, ${getInt(value, map)})"

    override def toString = s"allocDelayMem($name, $value)"
  }

  case object EOF extends Instruction {
    def run(map: Map[String, EquValue]) = ()
    def runString(map: Map[String, EquValue]) = ""
  }

  case class Skp(flags: MapsToInteger, nSkip: MapsToInteger) extends Instruction {
    def run(map: Map[String, EquValue]) = skip(getInt(flags, map), getInt(nSkip, map))
    def runString(map: Map[String, EquValue]) =
      s"skip(${getInt(flags, map)}, ${getInt(nSkip, map)})"

    override def toString: String = s"skip($flags, $nSkip)"
  }

  case object Clr extends Instruction {
    def run(map: Map[String, EquValue]) = clear()
    def runString(map: Map[String, EquValue]) = "clear()"

    override def toString: String = "clear()"
  }

  case class Exp(scale: MapsToDouble, offset: MapsToDouble) extends Instruction {
    def run(map: Map[String, EquValue]) = exp(getDouble(scale, map), getDouble(offset, map))
    def runString(map: Map[String, EquValue]) =
      s"exp(${getDouble(scale, map)}, ${getDouble(offset, map)})"

    override def toString: String = s"exp($scale, $offset)"
  }

  case class Mulx(addr: MapsToInteger) extends Instruction {
    def run(map: Map[String, EquValue]) = mulx(getInt(addr, map))
    def runString(map: Map[String, EquValue]) = s"mulx(${getInt(addr, map)})"

    override def toString: String = s"mulx($addr)"
  }

  case class Wldr(lfo: MapsToInteger, freq: MapsToInteger, amp: MapsToInteger) extends Instruction {
    def run(map: Map[String, EquValue]) =
      loadRampLFO(getInt(lfo, map), getInt(freq, map), getInt(amp, map))
    def runString(map: Map[String, EquValue]) =
      s"loadRampLFO(${getInt(lfo, map)}, ${getInt(freq, map)}, ${getInt(amp, map)})"

    override def toString: String = s"loadRampLFO($lfo, $freq, $amp)"
  }

  case class ChoRda(lfo: MapsToInteger, flags: MapsToInteger, addr: MapsToInteger)
      extends Instruction {
    def run(map: Map[String, EquValue]) =
      chorusReadDelay(getInt(lfo, map), getInt(flags, map), getInt(addr, map))
    def runString(map: Map[String, EquValue]) =
      s"chorusReadDelay(${getInt(lfo, map)}, ${getInt(flags, map)})"

    override def toString: String = s"chorusReadDelay($lfo, $flags, $addr)"
  }

  case class ChoSof(lfo: MapsToInteger, flags: MapsToInteger, offset: MapsToDouble)
      extends Instruction {
    def run(map: Map[String, EquValue]) =
      chorusScaleOffset(getInt(lfo, map), getInt(flags, map), getDouble(offset, map))
    def runString(map: Map[String, EquValue]) =
      s"chorusScaleOffset(${getInt(lfo, map)}, ${getInt(flags, map)}, ${getDouble(offset, map)})"

    override def toString: String = s"chorusReadDelay($lfo, $flags, $offset)"
  }

}

object Instruction {
  sealed trait Instruction {
    def run(map: Map[String, EquValue]): Unit
    def runString(map: Map[String, EquValue]): String
  }
}
