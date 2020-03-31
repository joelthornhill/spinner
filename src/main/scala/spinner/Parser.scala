package spinner

import spinner.ParserCombinator._

import scala.util.parsing.combinator.RegexParsers

trait Parser extends RegexParsers {
  private val wordRegex = """[a-zA-Z0-9_]+""".r
//  private val doubleRegex = """((?:[0-9]{1,10})(?:[.][0-9]{1,10})?)""".r

  private val doubleR = """(\d+(\.\d*))""".r ^^ (_.toDouble)
  private val intRegex = """\d+""".r ^^ (_.toDouble)

  private def singleWord: Parser[InstructionValue] = opt("-".r) ~ wordRegex ^^ {
    case Some(_) ~ word => WithArithmetic(Minus(StringValue(word)))
    case None ~ word    => StringValue(word)
  }

  private def singleDouble: Parser[InstructionValue] = opt("-".r) ~ (doubleR | intRegex) ^^ {
    case Some(_) ~ double => WithArithmetic(Minus(DoubleValue(double)))
    case None ~ double    => DoubleValue(double)
  }

  def doubleWithDivision: Parser[InstructionValue] =
    singleDouble ~ "/".r ~ opt(singleDouble) ~ opt(singleWord) ^^ {
      case double ~ _ ~ Some(divideBy) ~ None => WithArithmetic(Division(double, divideBy))
      case double ~ _ ~ None ~ Some(divideBy) => WithArithmetic(Division(double, divideBy))
      case double ~ _ ~ _ ~ _                 => double
    }

  def doubleWithAddition: Parser[InstructionValue] =
    singleDouble ~ "\\+".r ~ opt(singleDouble) ~ opt(singleWord) ^^ {
      case double ~ _ ~ Some(add) ~ None => WithArithmetic(Addition(double, add))
      case double ~ _ ~ None ~ Some(add) => WithArithmetic(Addition(double, add))
      case double ~ _ ~ _ ~ _            => double
    }

  def doubleWithMultiplication: Parser[InstructionValue] =
    singleDouble ~ "\\*".r ~ opt(singleDouble) ~ opt(singleWord) ^^ {
      case double ~ _ ~ Some(multiplier) ~ None =>
        WithArithmetic(Multiplication(double, multiplier))
      case double ~ _ ~ None ~ Some(multiplier) =>
        WithArithmetic(Multiplication(double, multiplier))
      case double ~ _ ~ _ ~ _ => double
    }

  def wordWithDivision: Parser[InstructionValue] =
    singleWord ~ "/".r ~ opt(singleDouble) ~ opt(singleWord) ^^ {
      case word ~ _ ~ Some(divideBy) ~ None => WithArithmetic(Division(word, divideBy))
      case word ~ _ ~ None ~ Some(divideBy) => WithArithmetic(Division(word, divideBy))
      case word ~ _ ~ _ ~ _                 => word
    }

  def wordWithAddition: Parser[InstructionValue] =
    singleWord ~ "\\+".r ~ opt(singleDouble) ~ opt(singleWord) ^^ {
      case word ~ _ ~ Some(add) ~ None => WithArithmetic(Addition(word, add))
      case word ~ _ ~ None ~ Some(add) => WithArithmetic(Addition(word, add))
      case word ~ _ ~ _ ~ _            => word
    }

  def wordWithMultiplication: Parser[InstructionValue] =
    singleWord ~ "\\*".r ~ opt(singleDouble) ~ opt(singleWord) ^^ {
      case word ~ _ ~ Some(multiplier) ~ None => WithArithmetic(Multiplication(word, multiplier))
      case word ~ _ ~ None ~ Some(multiplier) => WithArithmetic(Multiplication(word, multiplier))
      case word ~ _ ~ _ ~ _                   => word
    }

  def wordWithHash: Parser[InstructionValue] =
    wordRegex ~ "#".r ^^ {
      case word ~ _  => WithArithmetic(DelayEnd(StringValue(word)))
    }

  def wordWithCarat: Parser[InstructionValue] =
    wordRegex ~ "\\^".r ^^ {
      case word ~ _ => WithArithmetic(MidpointDelay(StringValue(word)))
    }

//  def wordWithCarat: Parser[InstructionValue] =
//    wordRegex ~ "\\^".r  ~ opt("\\+".r) ~ opt(singleDouble)  ^^ {
//      case word ~ _ ~ Some(_) ~ Some(add) => WithArithmetic(MidpointDelay(WithArithmetic(Addition(StringValue(word), add))))
//      case word ~ _ ~ _ ~ _ => WithArithmetic(MidpointDelay(StringValue(word)))
//    }


  def orWord: Parser[InstructionValue] =
    """([a-zA-Z0-9_]+\s?\|\s?)+[a-zA-Z0-9_]+""".r ^^ { word =>
      WithArithmetic(Or(word.split("\\|").toList.map(a => StringValue(a.trim))))
    }

  def word: Parser[InstructionValue] =
    orWord | wordWithDivision | wordWithAddition | wordWithMultiplication | wordWithHash | singleWord

  def double: Parser[InstructionValue] =
     doubleWithDivision | doubleWithAddition | doubleWithMultiplication | singleDouble
}
