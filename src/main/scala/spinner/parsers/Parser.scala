package spinner.parsers

import spinner.model._

import scala.util.parsing.combinator.RegexParsers

trait Parser extends RegexParsers {
  private val wordRegex = """[a-zA-Z0-9_]+""".r
  private val doubleParser = """(\d+(\.\d*))""".r ^^ (_.toDouble)
  private val intParser = """\d+""".r ^^ (_.toDouble)

  private def singleWord: Parser[InstructionValue] = opt("-".r) ~ wordRegex ^^ {
    case Some(_) ~ word => Minus(StringValue(word))
    case None ~ word    => StringValue(word)
  }

  private def singleDouble: Parser[InstructionValue] = opt("-".r) ~ (doubleParser | intParser) ^^ {
    case Some(_) ~ double => Minus(DoubleValue(double))
    case None ~ double    => DoubleValue(double)
  }

  def doubleWithDivision: Parser[InstructionValue] =
    singleDouble ~ "/".r ~ opt(singleDouble) ~ opt(singleWord) ^^ {
      case double ~ _ ~ Some(divideBy) ~ None => Division(double, divideBy)
      case double ~ _ ~ None ~ Some(divideBy) => Division(double, divideBy)
      case double ~ _ ~ _ ~ _                 => double
    }

  def doubleWithAddition: Parser[InstructionValue] =
    singleDouble ~ "\\+".r ~ opt(singleDouble) ~ opt(singleWord) ^^ {
      case double ~ _ ~ Some(add) ~ None => Addition(double, add)
      case double ~ _ ~ None ~ Some(add) => Addition(double, add)
      case double ~ _ ~ _ ~ _            => double
    }

  def doubleWithMinus: Parser[InstructionValue] =
    singleDouble ~ "-".r ~ opt(singleDouble) ~ opt(singleWord) ^^ {
      case double ~ _ ~ Some(add) ~ None => MinusValues(double, add)
      case double ~ _ ~ None ~ Some(add) => MinusValues(double, add)
      case double ~ _ ~ _ ~ _            => double
    }

  def doubleWithMultiplication: Parser[InstructionValue] =
    singleDouble ~ "\\*".r ~ opt(singleDouble) ~ opt(singleWord) ^^ {
      case double ~ _ ~ Some(multiplier) ~ None =>
        Multiplication(double, multiplier)
      case double ~ _ ~ None ~ Some(multiplier) =>
        Multiplication(double, multiplier)
      case double ~ _ ~ _ ~ _ => double
    }

  def wordWithDivision: Parser[InstructionValue] =
    singleWord ~ "/".r ~ opt(singleDouble) ~ opt(singleWord) ^^ {
      case word ~ _ ~ Some(divideBy) ~ None => Division(word, divideBy)
      case word ~ _ ~ None ~ Some(divideBy) => Division(word, divideBy)
      case word ~ _ ~ _ ~ _                 => word
    }

  def wordWithAddition: Parser[InstructionValue] =
    singleWord ~ "\\+".r ~ opt(singleDouble) ~ opt(singleWord) ^^ {
      case word ~ _ ~ Some(add) ~ None => Addition(word, add)
      case word ~ _ ~ None ~ Some(add) => Addition(word, add)
      case word ~ _ ~ _ ~ _            => word
    }

  def wordWithMultiplication: Parser[InstructionValue] =
    singleWord ~ "\\*".r ~ opt(singleDouble) ~ opt(singleWord) ^^ {
      case word ~ _ ~ Some(multiplier) ~ None =>
        Multiplication(word, multiplier)
      case word ~ _ ~ None ~ Some(multiplier) =>
        Multiplication(word, multiplier)
      case word ~ _ ~ _ ~ _ => word
    }

  def wordWithMinus: Parser[InstructionValue] =
    singleWord ~ "-".r ~ opt(singleWord) ~ opt(singleDouble) ^^ {
      case word ~ _ ~ Some(minus) ~ _ => MinusValues(word, minus)
      case word ~ _ ~ _ ~ Some(minus) => MinusValues(word, minus)
      case word ~ _ ~ _ ~ _           => word
    }

  def wordWithHash: Parser[InstructionValue] =
    wordRegex ~ "#".r ~ opt("-".r) ~ opt(wordRegex) ~ opt("-".r) ~ opt(intParser) ^^ {
      case word ~ _ ~ Some(_) ~ Some(wordMinus) ~ None ~ None =>
        DelayEnd(MinusValues(StringValue(word), StringValue(wordMinus)))
      case word ~ _ ~ Some(_) ~ Some(wordMinus) ~ Some(_) ~ Some(m) =>
        DelayEnd(
          MinusValues(StringValue(word), MinusValues(StringValue(wordMinus), DoubleValue(m)))
        )
      case word ~ _ ~ _ ~ _ ~ _ ~ _ => DelayEnd(StringValue(word))
    }

  def wordWithCarat: Parser[InstructionValue] =
    wordRegex ~ "\\^".r ^^ {
      case word ~ _ => MidpointDelay(StringValue(word))
    }

  def orWord: Parser[InstructionValue] =
    """([a-zA-Z0-9_]+\s?\|\s?)+[a-zA-Z0-9_]+""".r ^^ { word =>
      Or(word.split("\\|").toList.map(a => StringValue(a.trim)))
    }

  def binaryWord: Parser[InstructionValue] =
    "%" ~ "[0-9_]+".r ^^ {
      case _ ~ value => Binary(StringValue(value))
    }

  def hexWord: Parser[InstructionValue] =
    "$" ~ "[0-9a-fA-F]+".r ^^ {
      case _ ~ value => Hex(StringValue(value))
    }

  def word: Parser[InstructionValue] =
    orWord | wordWithDivision | wordWithAddition | wordWithMultiplication | wordWithMinus | wordWithHash | wordWithCarat | binaryWord | singleWord | hexWord

  def double: Parser[InstructionValue] =
    doubleWithDivision | doubleWithMinus | doubleWithAddition | doubleWithMultiplication | singleDouble

}
