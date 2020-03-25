package example

import example.EquParser.EquValue
import EquParser._

import scala.util.parsing.combinator._

trait EquParser extends RegexParsers {

  private val wordRegex = """[a-zA-Z0-9#_]+""".r
  private val doubleRegex = """-?[0-9]{1,10}([.][0-9]{0,10})?""".r
  private val integerRegex = """(-?[0-9]{1,10})""".r

  private def word: Parser[String] = wordRegex ^^ { _.toString }
  private def equWord: Parser[EquStringValue] = wordRegex ^^ (s => EquStringValue(s.toString))
  private def double: Parser[EquDoubleValue] = doubleRegex ^^ (d => EquDoubleValue(d.toDouble))
  private def integer: Parser[EquValue] = integerRegex ^^ (d => EquIntegerValue(d.toInt))

  private def stringOrIntOrDouble: Parser[EquValue] = integer | double | equWord
  private def stringOrInt: Parser[EquValue] = integer | equWord

  private def equStringParser: Parser[Map[String, EquValue]] =
    "equ".r ~ word ~ stringOrIntOrDouble ^^ {
      case _ ~ key ~ value => Map(key -> value)
    }

  private def memParser: Parser[Map[String, EquValue]] = "mem".r ~ word ~ stringOrInt ^^ {
    case _ ~ key ~ value => Map(key -> value)
  }

  private def anythingElse: Parser[Map[String, EquValue]] = """.*""".r ^^ (_ => Map.empty)

  def equParser: Parser[Map[String, EquValue]] = equStringParser | memParser | anythingElse
}

object EquParser {

  sealed trait EquValue
  case class EquStringValue(value: String) extends EquValue {
    override def toString: String = value
  }
  case class EquDoubleValue(value: Double) extends EquValue {
    override def toString: String = value.toString
  }
  case class EquIntegerValue(value: Int) extends EquValue {
    override def toString: String = value.toString
  }
}
