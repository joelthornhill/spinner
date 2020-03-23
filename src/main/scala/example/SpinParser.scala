package example
import example.EquParser.EquValue
import example.Instructions._
import example.ParserCombinator.{DoubleValue, InstructionValue, IntegerValue, StringValue}

import scala.util.parsing.combinator._

trait SpinParser extends RegexParsers with ReservedWordsParser {

  val map = Map[String, String](
    "krt" -> "reg0",
    "kin" -> "reg1",
    "kmix" -> "reg2"
  )

  private def word: Parser[String]   = """[a-z0-9-#]+""".r ^^ { _.toString }
  private def instruction: Parser[InstructionValue]   = """[a-z0-9-#]+""".r ^^ { a => StringValue(a.toString) }
  private def double: Parser[Double] = """-?[0-9]{1,10}([.][0-9]{0,10})?""".r ^^ { a =>
    a.toDouble
  }
  private def doubleInstruction: Parser[InstructionValue] = """-?[0-9]{1,10}([.][0-9]{0,10})?""".r ^^ { a =>
    DoubleValue(a.toDouble)
  }
  private def integer: Parser[Int] = """(-?[0-9]{0,10})""".r ^^ { _.toInt }
  private def integerInstruction: Parser[InstructionValue] = """(-?[0-9]{1,10})""".r ^^ { a => IntegerValue(a.toInt) }
  private def comma: Parser[String]  = """,""".r ^^ { _.toString }

  private def rdax: Parser[String]   = """rdax""".r ^^ { _.toString }
  private def rda: Parser[String]    = """rda""".r ^^ { _.toString }
  private def wrax: Parser[String]   = """wrax""".r ^^ { _.toString }
  private def wrap: Parser[String]   = """wrap""".r ^^ { _.toString }
  private def wra: Parser[String]    = """wra""".r ^^ { _.toString }
  private def mem: Parser[String]    = """mem""".r ^^ { _.toString }
  private def equ: Parser[String]    = """equ""".r ^^ { _.toString }
  private def sof: Parser[String]    = """sof""".r ^^ { _.toString }

  private def reservedOrStringOrDouble: Parser[InstructionValue] = reservedWords | doubleInstruction | instruction
  private def reservedOrStringOrInt: Parser[InstructionValue] = reservedWords | integerInstruction | instruction

  private def equParser: Parser[Instruction] = equ ~ word ~ word ^^ {
    case _ ~ name ~ value => Equ(name, value)
  }

  private def rdaxInstruction: Parser[Instruction] = rdax ~ reservedOrStringOrInt ~ comma ~ reservedOrStringOrDouble ^^ {
    case _ ~ reservedOrStringOrInt ~ _ ~ scale => Rdax(reservedOrStringOrInt, scale)
  }

  private def rdaInstruction: Parser[Instruction] = rda ~ reservedOrStringOrInt ~ comma ~ reservedOrStringOrDouble ^^ {
    case _ ~ reservedOrStringOrInt ~ _ ~ scale => Rda(reservedOrStringOrInt, scale)
  }

  private def wraxInstruction: Parser[Instruction] = wrax ~ reservedOrStringOrInt ~ comma ~ reservedOrStringOrDouble ^^ {
    case _ ~ reservedOrStringOrInt ~ _ ~ scale => Wrax(reservedOrStringOrInt, scale)
  }

  private def memInstruction: Parser[Instruction] = mem ~ word ~ integer ^^ {
    case _ ~ word ~ integer => Mem(word, integer)
  }

  private def sofInstruction: Parser[Instruction] = sof ~ double ~ comma ~ double ^^ {
    case _ ~ d1 ~ _ ~ d2 => Sof(d1, d2)
  }

  private def wrapInstruction: Parser[Instruction] = wrap ~ reservedOrStringOrInt ~ comma ~ reservedOrStringOrDouble ^^ {
    case _ ~ reservedOrStringOrInt ~ _ ~ double => Wrap(reservedOrStringOrInt, double)
  }

  private def wraInstruction: Parser[Instruction] = wra ~ reservedOrStringOrInt ~ comma ~ reservedOrStringOrDouble ^^ {
    case _ ~ reservedOrStringOrInt ~ _ ~ reservedOrStringOrDouble => Wra(reservedOrStringOrInt, reservedOrStringOrDouble)
  }

  def spinParser: Parser[Instruction] = rdaxInstruction | rdaxInstruction | wraxInstruction | memInstruction | sofInstruction | wrapInstruction | equParser | rdaInstruction | wraInstruction
}


case object ParserCombinator {

  sealed trait InstructionValue
  case class StringValue(value: String) extends InstructionValue
  case class DoubleValue(value: Double) extends InstructionValue
  case class IntegerValue(value: Int) extends InstructionValue
  case class Reserved(reservedWord: ReservedWord) extends InstructionValue
}