package example
import example.EquParser.{EquDoubleValue, EquStringValue, EquValue}
import example.ParserCombinator.{DoubleValue, InstructionValue, Reserved, StringValue}
import org.andrewkilpatrick.elmGen.ElmProgram

object Instructions extends ElmProgram("blah") {

  sealed trait Instruction {
    def run(map: Map[String, EquValue]): Unit
  }

  def getInt(addr: InstructionValue, map: Map[String, EquValue]) = addr match {
    case r: Reserved => r.reservedWord.value
    case s: StringValue => map.get(s.value).map {
      case v: EquStringValue => ReservedWord.withName(v.s.toUpperCase).value
      case _ => ???
    }.getOrElse(0)
  }

  def getDouble(addr: InstructionValue, map: Map[String, EquValue]): Double = addr match {
    case d: DoubleValue => d.value
    case s: StringValue => map.get(s.value).map {
      case v: EquStringValue => ReservedWord.withName(v.s.toUpperCase).value
      case _ => ???
    }.getOrElse(0)
  }


  case class Rdax(addr: InstructionValue, scale: InstructionValue) extends Instruction {
    def run(map: Map[String, EquValue]) = readRegister(getAddr(addr, map), getAddr(scale, map))

    override def toString = s"readRegister(${addr.toString}, $scale)"
  }

  case class Rda(addr: InstructionValue, scale: InstructionValue) extends Instruction {
    def run(map: Map[String, EquValue]) = readDelay(getAddr(addr, map), getAddr(scale, map))

    override def toString: String = s"readDelay($addr, $scale)"
  }

  case class Wrax(addr: InstructionValue, scale: InstructionValue) extends Instruction {
    def run(map: Map[String, EquValue]) = writeRegister(getAddr(addr, map), getAddr(scale, map))

    override def toString = s"writeRegister($addr, $scale)"
  }

  case class Wrap(addr: InstructionValue, scale: InstructionValue) extends Instruction {
    def run(map: Map[String, EquValue]) = writeAllpass(getAddr(addr, map), getAddr(scale, map))

    override def toString: String = s"writeAllpass($addr, $scale)"
  }

  case class Wra(addr: InstructionValue, scale: InstructionValue) extends Instruction {
    def run(map: Map[String, EquValue]) = writeRegister(getAddr(addr, map), getAddr(scale, map))

    override def toString: String = s"writeAllpass($addr, $scale)"
  }

  case class Sof(scale: InstructionValue, offset: InstructionValue) extends Instruction {
    def run(map: Map[String, EquValue]) = ??? //scaleOffset(scale, offset)

    override def toString: String = s"scaleOffset($scale, $offset)"
  }

  case class Equ[T](name: String, value: T) extends Instruction {
    def run(map: Map[String, EquValue]) = () // Do nothing
    override def toString = s"val $name = $value"
  }

  case class Mem(name: String, value: Int ) extends Instruction {
    def run(map: Map[String, EquValue]) = allocDelayMem(name, value)

    override def toString = s"allocDelayMem($name, $value)"
  }

  case object EOF extends Instruction {
    def run(map: Map[String, EquValue]) = ()
  }

}
