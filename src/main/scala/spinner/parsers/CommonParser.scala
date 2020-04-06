package spinner.parsers

import spinner.model.InstructionValue
import spinner.model.StringValue

import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

trait CommonParser extends RegexParsers with Parser {

  val wordRegex: Regex = """[a-zA-Z0-9_]+""".r

  def comma: Parser[String] = "," ^^ (_.toString)

  def loop: Parser[InstructionValue] = "loop" ^^ { a => StringValue(a.toString) }

  def StringOrDouble: Parser[InstructionValue] =
    double | word
}
