package spinner

import spinner.ParserCombinator.InstructionValue

import scala.util.parsing.combinator._

trait EquParser extends RegexParsers with Parser {
  private val wordRegex = """[a-zA-Z0-9#_]+""".r

  private def equStringParser: Parser[Map[String, InstructionValue]] =
    "equ".r ~ wordRegex ~ (double | word) ^^ {
      case _ ~ key ~ value => Map(key -> value)
    }

  private def memParser: Parser[Map[String, InstructionValue]] =
    "mem".r ~ wordRegex ~ (double | word) ^^ {
      case _ ~ key ~ value => Map(key -> value)
    }

  private def anythingElse: Parser[Map[String, InstructionValue]] = """.*""".r ^^ (_ => Map.empty)

  def equParser: Parser[Map[String, InstructionValue]] = equStringParser | memParser | anythingElse
}
