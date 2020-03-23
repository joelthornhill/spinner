package example

import example.EquParser.EquValue
import EquParser._

import scala.util.parsing.combinator._

trait EquParser extends RegexParsers {

  private def word: Parser[String]   = """[a-z0-9]+""".r ^^ { _.toString }
  private def equWord: Parser[EquValue]   = """[a-z0-9]+""".r ^^ { s => EquStringValue(s.toString) }
  private def double: Parser[EquValue] = """-?[0-9]{0,10}([.][0-9]{0,10})?""".r ^^ { d => EquDoubleValue(d.toDouble) }
  private def equ: Parser[String]    = """equ""".r ^^ { _.toString }
  private def stringOrDouble: Parser[EquValue] = equWord | double
  private def equStringInstruction: Parser[Map[String, EquValue]] = equ ~ word ~ stringOrDouble ^^ {
    case _ ~ key ~ word => Map(key -> word)
  }

  private def anythingElse: Parser[Map[String, EquValue]] =  """.*""".r ^^ { _ => Map.empty }

  def equParser: Parser[Map[String, EquValue]] = equStringInstruction | anythingElse
}

object EquParser {

  sealed trait EquValue
  case class EquStringValue(s: String) extends EquValue
  case class EquDoubleValue(d: Double) extends EquValue
}