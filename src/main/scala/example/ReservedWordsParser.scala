package example
import example.ParserCombinator.Reserved

import scala.util.Try
import scala.util.parsing.combinator.RegexParsers

trait ReservedWordsParser extends RegexParsers {

  def reservedWords: Parser[Reserved] = """[a-zA-Z0-9_]+""".r ^? {
    case v if Try(ReservedWord.withName(v.toUpperCase)).isSuccess =>
      Reserved(ReservedWord.withName(v.toUpperCase))
  }

}
