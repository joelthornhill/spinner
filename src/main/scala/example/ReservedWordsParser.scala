package example
import enumeratum._
import example.ParserCombinator.Reserved
import example.ReservedWord._

import scala.util.Try
import scala.util.parsing.combinator

trait ReservedWordsParser extends combinator.RegexParsers {
  // TODO Update regex
  def reservedWords: Parser[Reserved] = """[a-z0-9]+""".r ^? {
    case v if Try(ReservedWord.withName(v.toUpperCase)).isSuccess =>
      Reserved(ReservedWord.withName(v.toUpperCase))
  }
}

object ReservedWordsParser {

}
