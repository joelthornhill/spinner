package spinner.parsers

import spinner.model
import spinner.model._

import scala.util.parsing.combinator.RegexParsers

trait Parser extends RegexParsers {
  private val wordRegex = """[a-zA-Z0-9_]+""".r
  private val doubleParser = """(\d+(\.\d*))""".r ^^ (_.toDouble)
  private val intParser = """\d+""".r ^^ (_.toDouble)

  private def singleWord: Parser[InstructionValue] = opt("-".r) ~ wordRegex ^^ {
    case Some(_) ~ word => WithArithmetic(Minus(StringValue(word)))
    case None ~ word    => StringValue(word)
  }

  private def singleDouble: Parser[InstructionValue] = opt("-".r) ~ (doubleParser | intParser) ^^ {
    case Some(_) ~ double => WithArithmetic(model.Minus(DoubleValue(double)))
    case None ~ double    => DoubleValue(double)
  }

  def doubleWithDivision: Parser[InstructionValue] =
    singleDouble ~ "/".r ~ opt(singleDouble) ~ opt(singleWord) ^^ {
      case double ~ _ ~ Some(divideBy) ~ None => WithArithmetic(Division(double, divideBy))
      case double ~ _ ~ None ~ Some(divideBy) => WithArithmetic(model.Division(double, divideBy))
      case double ~ _ ~ _ ~ _                 => double
    }

  def doubleWithAddition: Parser[InstructionValue] =
    singleDouble ~ "\\+".r ~ opt(singleDouble) ~ opt(singleWord) ^^ {
      case double ~ _ ~ Some(add) ~ None => WithArithmetic(Addition(double, add))
      case double ~ _ ~ None ~ Some(add) => WithArithmetic(model.Addition(double, add))
      case double ~ _ ~ _ ~ _            => double
    }

  def doubleWithMinus: Parser[InstructionValue] =
    singleDouble ~ "-".r ~ opt(singleDouble) ~ opt(singleWord) ^^ {
      case double ~ _ ~ Some(add) ~ None => WithArithmetic(model.MinusValues(double, add))
      case double ~ _ ~ None ~ Some(add) => WithArithmetic(model.MinusValues(double, add))
      case double ~ _ ~ _ ~ _            => double
    }

  def doubleWithMultiplication: Parser[InstructionValue] =
    singleDouble ~ "\\*".r ~ opt(singleDouble) ~ opt(singleWord) ^^ {
      case double ~ _ ~ Some(multiplier) ~ None =>
        WithArithmetic(Multiplication(double, multiplier))
      case double ~ _ ~ None ~ Some(multiplier) =>
        WithArithmetic(model.Multiplication(double, multiplier))
      case double ~ _ ~ _ ~ _ => double
    }

  def wordWithDivision: Parser[InstructionValue] =
    singleWord ~ "/".r ~ opt(singleDouble) ~ opt(singleWord) ^^ {
      case word ~ _ ~ Some(divideBy) ~ None => WithArithmetic(model.Division(word, divideBy))
      case word ~ _ ~ None ~ Some(divideBy) => WithArithmetic(model.Division(word, divideBy))
      case word ~ _ ~ _ ~ _                 => word
    }

  def wordWithAddition: Parser[InstructionValue] =
    singleWord ~ "\\+".r ~ opt(singleDouble) ~ opt(singleWord) ^^ {
      case word ~ _ ~ Some(add) ~ None => WithArithmetic(model.Addition(word, add))
      case word ~ _ ~ None ~ Some(add) => WithArithmetic(model.Addition(word, add))
      case word ~ _ ~ _ ~ _            => word
    }

  def wordWithMultiplication: Parser[InstructionValue] =
    singleWord ~ "\\*".r ~ opt(singleDouble) ~ opt(singleWord) ^^ {
      case word ~ _ ~ Some(multiplier) ~ None =>
        WithArithmetic(model.Multiplication(word, multiplier))
      case word ~ _ ~ None ~ Some(multiplier) =>
        WithArithmetic(model.Multiplication(word, multiplier))
      case word ~ _ ~ _ ~ _ => word
    }

  def wordWithMinus: Parser[InstructionValue] =
    singleWord ~ "-".r ~ opt(singleWord) ~ opt(singleDouble) ^^ {
      case word ~ _ ~ Some(minus) ~ _ => WithArithmetic(model.MinusValues(word, minus))
      case word ~ _ ~ _ ~ Some(minus) => WithArithmetic(model.MinusValues(word, minus))
      case word ~ _ ~ _ ~ _           => word
    }

  def wordWithHash: Parser[InstructionValue] =
    wordRegex ~ "#".r ~ opt("-".r) ~ opt(wordRegex) ~ opt("-".r) ~ opt(intParser) ^^ {
      case word ~ _ ~ Some(_) ~ Some(wordMinus) ~ None ~ None =>
        WithArithmetic(
          DelayEnd(WithArithmetic(MinusValues(StringValue(word), StringValue(wordMinus))))
        )
      case word ~ _ ~ Some(_) ~ Some(wordMinus) ~ Some(_) ~ Some(m) =>
        WithArithmetic(
          DelayEnd(
            WithArithmetic(
              MinusValues(
                StringValue(word),
                WithArithmetic(MinusValues(StringValue(wordMinus), DoubleValue(m)))
              )
            )
          )
        )
      case word ~ _ ~ _ ~ _ ~ _ ~ _ => WithArithmetic(DelayEnd(StringValue(word)))
    }

  def wordWithCarat: Parser[InstructionValue] =
    wordRegex ~ "\\^".r ^^ {
      case word ~ _ => WithArithmetic(MidpointDelay(StringValue(word)))
    }

  def orWord: Parser[InstructionValue] =
    """([a-zA-Z0-9_]+\s?\|\s?)+[a-zA-Z0-9_]+""".r ^^ { word =>
      WithArithmetic(Or(word.split("\\|").toList.map(a => StringValue(a.trim))))
    }

  def binaryWord: Parser[InstructionValue] =
    "%" ~ "[0-9_]+".r ^^ {
      case _ ~ value => WithArithmetic(Binary(StringValue(value)))
    }

  def hexWord: Parser[InstructionValue] =
    "$" ~ "[0-9a-fA-F]+".r ^^ {
      case _ ~ value => WithArithmetic(Hex(StringValue(value)))
    }

  def word: Parser[InstructionValue] =
    orWord | wordWithDivision | wordWithAddition | wordWithMultiplication | wordWithMinus | wordWithHash | wordWithCarat | binaryWord | singleWord | hexWord

  def double: Parser[InstructionValue] =
    doubleWithDivision | doubleWithMinus | doubleWithAddition | doubleWithMultiplication | singleDouble

}
