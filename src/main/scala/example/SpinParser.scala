package example
import example.Instruction.Instruction
import example.ParserCombinator._

import scala.util.parsing.combinator._

trait CommonParsers extends RegexParsers with ReservedWordsParser with Parser {

  val wordRegex = """[a-zA-Z0-9_]+""".r

  def comma: Parser[String] = ",".r ^^ (_.toString)

  // TODO: handle $ correctly
  def hash: Parser[String] = "#" ^^ (_.toString)

  def bar: Parser[String] = "|" ^^ (_.toString)

  // TODO: Handle Ors correctly
  def barWord: Parser[Option[String ~ String]] = opt(bar ~ wordRegex)

  def loop: Parser[InstructionValue] = "loop".r ^^ { a => StringValue(a.toString) }

  def reservedOrStringOrDouble: Parser[InstructionValue] =
    reservedWords | double | word
}

trait SpinParser[F[_]] extends RegexParsers with CommonParsers with ReservedWordsParser {

  def parsed(instructions: Instructions[F]): Parser[Instruction[F]] = {

    def eof: Parser[Instruction[F]] = """^\s*$""".r ^^ (_ => instructions.EOF)

    // Instructions
    // TODO: maxx, absa, log, and, or, xor, jam, nop, not
    def rdax: Parser[(InstructionValue, InstructionValue) => instructions.Rdax] =
      "rdax".r ^^ (_ => instructions.Rdax)
    def rda: Parser[(String, Double, InstructionValue) => instructions.Rda] =
      "rda".r ^^ (_ => instructions.Rda)
    def wrax: Parser[(InstructionValue, InstructionValue) => instructions.Wrax] =
      "wrax".r ^^ (_ => instructions.Wrax)
    def wrap: Parser[(String, Double, InstructionValue) => instructions.Wrap] =
      "wrap".r ^^ (_ => instructions.Wrap)
    def wra: Parser[(String, Double, InstructionValue) => instructions.Wra] =
      "wra".r ^^ (_ => instructions.Wra)
    def wra2: Parser[(InstructionValue, InstructionValue) => instructions.Wra2] =
      "wra".r ^^ (_ => instructions.Wra2)
    def sof: Parser[(InstructionValue, InstructionValue) => instructions.Sof] =
      "sof".r ^^ (_ => instructions.Sof)
    def exp: Parser[(InstructionValue, InstructionValue) => instructions.Exp] =
      "exp".r ^^ (_ => instructions.Exp)
    def rdfx: Parser[(InstructionValue, InstructionValue) => instructions.Rdfx] =
      "rdfx".r ^^ (_ => instructions.Rdfx)

    def rmpa: Parser[Instruction[F]] = "rmpa".r ~ reservedOrStringOrDouble ^^ {
      case _ ~ lfo => instructions.Rmpa(lfo)
    }

    def log: Parser[(InstructionValue, InstructionValue) => instructions.Log] =
      "log".r ^^ (_ => instructions.Log)

    def memParser: Parser[Instruction[F]] = "mem".r ~ wordRegex ~ reservedOrStringOrDouble ^^ {
      case _ ~ word ~ integer => instructions.Mem(word, integer)
    }

    def equParser: Parser[Instruction[F]] = "equ".r ~ wordRegex ~ reservedOrStringOrDouble ^^ {
      case _ ~ name ~ value => instructions.Equ(name, value)
    }

    def skpParser: Parser[Instruction[F]] =
      "skp".r ~ reservedOrStringOrDouble ~ comma ~ reservedOrStringOrDouble ^^ {
        case _ ~ flags ~ _ ~ nSkip => instructions.Skp(flags, nSkip)
      }

    def mulxParser: Parser[Instruction[F]] = "mulx".r ~ reservedOrStringOrDouble ^^ {
      case _ ~ addr => instructions.Mulx(addr)
    }

    def wlds: Parser[(InstructionValue, InstructionValue, InstructionValue) => instructions.Wlds] =
      "wlds".r ^^ (_ => instructions.Wlds)

    def wldr: Parser[(InstructionValue, InstructionValue, InstructionValue) => instructions.Wldr] =
      "wldr".r ^^ (_ => instructions.Wldr)

    def wrlx: Parser[(InstructionValue, InstructionValue) => instructions.Wrlx] =
      "wrlx".r ^^ (_ => instructions.Wrlx)

    def wrhx: Parser[(InstructionValue, InstructionValue) => instructions.Wrhx] =
      "wrhx".r ^^ (_ => instructions.Wrhx)

    def andParser: Parser[Instruction[F]] = "and".r ~ reservedOrStringOrDouble ^^ {
      case _ ~ mask => instructions.And(mask)
    }

    def orParser: Parser[Instruction[F]] = "or".r ~ reservedOrStringOrDouble ^^ {
      case _ ~ mask => instructions.Or(mask)
    }

    def clr: Parser[Instruction[F]] = "clr".r ^^ (_ => instructions.Clr)

    def choRda: Parser[Instruction[F]] =
      "cho".r ~ "rda".r ~ comma ~ reservedOrStringOrDouble ~ comma ~ reservedOrStringOrDouble ~ barWord ~ barWord ~ comma ~ reservedOrStringOrDouble ~ barWord ^^ {
        case _ ~ _ ~ _ ~ lfo ~ _ ~ flags ~ _ ~ _ ~ _ ~ addr ~ _ =>
          instructions.ChoRda(lfo, flags, addr)
      }
    def choSfo: Parser[Instruction[F]] =
      "cho".r ~ "sof".r ~ comma ~ reservedOrStringOrDouble ~ comma ~ reservedOrStringOrDouble ~ barWord ~ comma ~ reservedOrStringOrDouble ~ barWord ^^ {
        case _ ~ _ ~ _ ~ lfo ~ _ ~ flags ~ _ ~ _ ~ offset ~ _ =>
          instructions.ChoSof(lfo, flags, offset)
      }

    def choRdal: Parser[Instruction[F]] =
      "cho".r ~ "rdal".r ~ comma ~ reservedOrStringOrDouble ^^ {
        case _ ~ _ ~ _ ~ lfo => instructions.ChoRdal(lfo)
      }

    def choParser = choRda | choSfo | choRdal

    def paramDoubleParamDouble: Parser[Instruction[F]] =
      (rdax | wrax | rdfx | wrhx | wrlx | sof | exp | log | wra2) ~ reservedOrStringOrDouble ~ comma ~ reservedOrStringOrDouble ^^ {
        case instruction ~ d1 ~ _ ~ d2 => instruction(d1, d2)
      }

    def paramStringDoubleDouble: Parser[Instruction[F]] =
      (rda | wrap | wra) ~ wordRegex ~ opt(hash) ~ comma ~ reservedOrStringOrDouble ^^ {
        case instruction ~ word ~ hash ~ _ ~ double =>
          instruction(word, hash.map(_ => 1.0).getOrElse(0.0), double)
      }

    def paramDoubleDoubleDouble: Parser[Instruction[F]] =
      (wlds | wldr) ~ reservedOrStringOrDouble ~ comma ~ reservedOrStringOrDouble ~ comma ~ reservedOrStringOrDouble ^^ {
        case instruction ~ lfo ~ _ ~ freq ~ _ ~ amp =>
          instruction(lfo, freq, amp)
      }

    def loopParser: Parser[Instruction[F]] = "loop".r ^^ (_ => instructions.Loop)

    paramDoubleParamDouble | memParser | paramStringDoubleDouble | paramDoubleDoubleDouble | equParser | skpParser | clr | mulxParser | andParser | orParser | choParser | rmpa | loopParser | eof

  }
}

object ParserCombinator {

  sealed trait InstructionValue

  case class DoubleValue(value: Double) extends InstructionValue

  case class StringValue(value: String) extends InstructionValue
  case class Reserved(reservedWord: ReservedWord) extends InstructionValue
  case class WithArithmetic(value: Arithmetic) extends InstructionValue

  sealed trait Arithmetic
  case class Division(a: InstructionValue, b: InstructionValue) extends Arithmetic
  case class Addition(a: InstructionValue, b: InstructionValue) extends Arithmetic
  case class Minus(a: InstructionValue) extends Arithmetic
  case class Multiplication(a: InstructionValue, b: InstructionValue) extends Arithmetic
  case class DelayEnd(a: InstructionValue) extends Arithmetic
  case class MidpointDelay(a: InstructionValue) extends Arithmetic
  case class Or(a: List[InstructionValue]) extends Arithmetic
}
