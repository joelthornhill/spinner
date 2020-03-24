package example

import example.EquParser.EquValue
import EquParser._

import scala.util.parsing.combinator._

trait EquParser extends RegexParsers {

  private val wordRegex = """[a-z0-9-#_]+""".r
  private val doubleRegex = """-?[0-9]{1,10}([.][0-9]{0,10})?""".r
  private val integerRegex = """(-?[0-9]{1,10})""".r

  private def word: Parser[String]   = wordRegex ^^ { _.toString }
  private def equWord: Parser[EquStringValue]   = wordRegex ^^ ( s => EquStringValue(s.toString) )
  private def double: Parser[EquDoubleValue] = doubleRegex ^^ ( d => EquDoubleValue(d.toDouble) )
  private def integer: Parser[EquValue] = integerRegex ^^ ( d => EquIntegerValue(d.toInt) )
  private def equ: Parser[String]    = "equ".r ^^ ( _.toString )
  def mem: Parser[String] = "mem".r ^^ ( _.toString )
  private def stringOrDouble: Parser[EquValue] = double | equWord
  private def stringOrInt: Parser[EquValue] = integer | equWord

  private def equStringInstruction: Parser[Map[String, EquValue]] = equ ~ word ~ stringOrDouble ^^ {
    case _ ~ key ~ value => Map(key -> value)
  }

  private def memParser: Parser[Map[String, EquValue]] = mem ~ word ~ stringOrInt ^^ {
    case _ ~ key ~ value => Map(key -> value)
  }

  private def anythingElse: Parser[Map[String, EquValue]] =  """.*""".r ^^ (_ => Map.empty )

  def equParser: Parser[Map[String, EquValue]] = equStringInstruction | memParser | anythingElse
}

object EquParser {

  sealed trait EquValue
  case class EquStringValue(s: String) extends EquValue {
    override def toString: String = s
  }
  case class EquDoubleValue(d: Double) extends EquValue {
    override def toString: String = d.toString
  }
  case class EquIntegerValue(i: Int) extends EquValue {
    override def toString: String = i.toString
  }
}