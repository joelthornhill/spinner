package example
import example.Instruction.Instruction
import example.ParserCombinator._

import scala.util.Try
import scala.util.parsing.combinator._

trait CommonParsers extends RegexParsers with ReservedWordsParser {

  val wordRegex = """[a-zA-Z0-9-_+]+""".r
  val integerRegex = """(-?[0-9]{1,10})""".r
  val doubleRegex = """-?[0-9]{1,10}([.][0-9]{0,10})?""".r

  def word: Parser[String] = wordRegex ^^ { _.toString }
  def comma: Parser[String] = ",".r ^^ (_.toString)
  def instruction: Parser[StringValue] = wordRegex ^^ { a => StringValue(a.toString) }
  def doubleInstruction: Parser[DoubleValue] =
    doubleRegex ^^ (d => DoubleValue(d.toDouble))
  def integerInstruction: Parser[MapsToInteger] = integerRegex ^^ { a => IntegerValue(a.toInt) }

  // TODO: handle $ correctly
  def hash: Parser[String] = "#" ^^ (_.toString)

  def bar: Parser[String] = "|" ^^ (_.toString)

  // TODO: Handle Ors correctly
  def barWord: Parser[Option[String ~ String]] = opt(bar ~ word)

  def loop: Parser[MapsToInteger] = "loop".r ^^ { a => StringValue(a.toString) }

  def reservedOrStringOrDouble: Parser[MapsToDouble] =
    reservedWords | doubleInstruction | instruction
  def reservedOrStringOrInt: Parser[MapsToInteger] =
    reservedWords | integerInstruction | instruction
  def reservedOrStringOrIntOrLoop = reservedOrStringOrInt | loop

}

trait ReservedWordsParser extends RegexParsers {

  def reservedWords: Parser[Reserved] = """[a-zA-Z0-9_]+""".r ^? {
    case v if Try(ReservedWord.withName(v.toUpperCase)).isSuccess =>
      Reserved(ReservedWord.withName(v.toUpperCase))
  }

}

trait SpinParser[F[_]] extends RegexParsers with CommonParsers with ReservedWordsParser {

  def parsed(instructions: Instructions[F]): Parser[Instruction[F]] = {

    def eof: Parser[Instruction[F]] = """^\s*$""".r ^^ (_ => instructions.EOF)

    // Instructions
    // TODO: maxx, absa, log, and, or, xor, jam, nop, not
    def rdax: Parser[(MapsToInteger, MapsToDouble) => instructions.Rdax] =
      "rdax".r ^^ (_ => instructions.Rdax)
    def rda: Parser[(String, Double, MapsToDouble) => instructions.Rda] =
      "rda".r ^^ (_ => instructions.Rda)
    def wrax: Parser[(MapsToInteger, MapsToDouble) => instructions.Wrax] =
      "wrax".r ^^ (_ => instructions.Wrax)
    def wrap: Parser[(String, Double, MapsToDouble) => instructions.Wrap] =
      "wrap".r ^^ (_ => instructions.Wrap)
    def wra: Parser[(String, Double, MapsToDouble) => instructions.Wra] =
      "wra".r ^^ (_ => instructions.Wra)
    def sof: Parser[(MapsToDouble, MapsToDouble) => instructions.Sof] =
      "sof".r ^^ (_ => instructions.Sof)
    def exp: Parser[(MapsToDouble, MapsToDouble) => instructions.Exp] =
      "exp".r ^^ (_ => instructions.Exp)
    def rdfx: Parser[(MapsToInteger, MapsToDouble) => instructions.Rdfx] =
      "rdfx".r ^^ (_ => instructions.Rdfx)

    def rmpa: Parser[Instruction[F]] = "rmpa".r ~ reservedOrStringOrDouble ^^ {
      case _ ~ lfo => instructions.Rmpa(lfo)
    }

    def log: Parser[(MapsToDouble, MapsToDouble) => instructions.Log] =
      "log".r ^^ (_ => instructions.Log)

    def memParser: Parser[Instruction[F]] = "mem".r ~ word ~ reservedOrStringOrInt ^^ {
      case _ ~ word ~ integer => instructions.Mem(word, integer)
    }

    def equParser: Parser[Instruction[F]] = "equ".r ~ word ~ word ^^ {
      case _ ~ name ~ value => instructions.Equ(name, value)
    }

    def skpParser: Parser[Instruction[F]] =
      "skp".r ~ reservedOrStringOrInt ~ comma ~ reservedOrStringOrIntOrLoop ^^ {
        case _ ~ flags ~ _ ~ nSkip => instructions.Skp(flags, nSkip)
      }

    def mulxParser: Parser[Instruction[F]] = "mulx".r ~ reservedOrStringOrInt ^^ {
      case _ ~ addr => instructions.Mulx(addr)
    }

    def wldrParser: Parser[Instruction[F]] =
      "wldr".r ~ reservedOrStringOrInt ~ comma ~ reservedOrStringOrInt ~ comma ~ reservedOrStringOrInt ^^ {
        case _ ~ lfo ~ _ ~ freq ~ _ ~ amp =>
          instructions.Wldr(lfo, freq, amp)
      }

    def wrlx: Parser[(MapsToInteger, MapsToDouble) => instructions.Wrlx] =
      "wrlx".r ^^ (_ => instructions.Wrlx)

    def wrhx: Parser[(MapsToInteger, MapsToDouble) => instructions.Wrhx] =
      "wrhx".r ^^ (_ => instructions.Wrhx)

    def wldsParser: Parser[Instruction[F]] =
      "wlds".r ~ reservedOrStringOrInt ~ comma ~ reservedOrStringOrDouble ~ comma ~ reservedOrStringOrDouble ^^ {
        case _ ~ lfo ~ _ ~ freq ~ _ ~ amp => instructions.Wlds(lfo, freq, amp)
      }

    def andParser: Parser[Instruction[F]] = "and".r ~ reservedOrStringOrInt ^^ {
      case _ ~ mask => instructions.And(mask)
    }

    def orParser: Parser[Instruction[F]] = "or".r ~ reservedOrStringOrInt ^^ {
      case _ ~ mask => instructions.Or(mask)
    }

    def cho: Parser[String] = "cho".r ^^ (_.toString)
    def clr: Parser[Instruction[F]] = "clr".r ^^ (_ => instructions.Clr)

    def choRda: Parser[Instruction[F]] =
      cho ~ "rda".r ~ comma ~ reservedOrStringOrInt ~ comma ~ reservedOrStringOrInt ~ barWord ~ barWord ~ comma ~ reservedOrStringOrInt ~ barWord ^^ {
        case _ ~ _ ~ _ ~ lfo ~ _ ~ flags ~ _ ~ _ ~ _ ~ addr ~ _ =>
          instructions.ChoRda(lfo, flags, addr)
      }
    def choSfo: Parser[Instruction[F]] =
      cho ~ "sof".r ~ comma ~ reservedOrStringOrInt ~ comma ~ reservedOrStringOrInt ~ barWord ~ comma ~ reservedOrStringOrDouble ~ barWord ^^ {
        case _ ~ _ ~ _ ~ lfo ~ _ ~ flags ~ _ ~ _ ~ offset ~ _ =>
          instructions.ChoSof(lfo, flags, offset)
      }

    def choRdal: Parser[Instruction[F]] =
      cho ~ "rdal".r ~ comma ~ reservedOrStringOrInt ^^ {
        case _ ~ _ ~ _ ~ lfo => instructions.ChoRdal(lfo)
      }

    def choParser = choRda | choSfo | choRdal

    def paramIntParamDouble: Parser[Instruction[F]] =
      (rdax | wrax | rdfx | wrhx | wrlx) ~ reservedOrStringOrInt ~ comma ~ reservedOrStringOrDouble ^^ {
        case instruction ~ int ~ _ ~ double => instruction(int, double)
      }

    def paramDoubleParamDouble: Parser[Instruction[F]] =
      (sof | exp | log) ~ reservedOrStringOrDouble ~ comma ~ reservedOrStringOrDouble ^^ {
        case instruction ~ d1 ~ _ ~ d2 => instruction(d1, d2)
      }

    def paramStringDoubleDouble: Parser[Instruction[F]] =
      (rda | wrap | wra) ~ word ~ opt(hash) ~ comma ~ reservedOrStringOrDouble ^^ {
        case instruction ~ word ~ hash ~ _ ~ double =>
          instruction(word, hash.map(_ => 1.0).getOrElse(0.0), double)
      }

    def loopParser: Parser[Instruction[F]] = "loop".r ^^ (_ => instructions.Loop)

    paramIntParamDouble | paramDoubleParamDouble | memParser | paramStringDoubleDouble | equParser | skpParser | clr | mulxParser | wldrParser | andParser | orParser | wldsParser | choParser | rmpa | loopParser | eof

  }
}

object ParserCombinator {

  sealed trait InstructionValue
  sealed trait MapsToInteger extends InstructionValue
  sealed trait MapsToDouble extends InstructionValue

  case class DoubleValue(value: Double) extends MapsToDouble
  case class IntegerValue(value: Int) extends MapsToInteger

  case class StringValue(value: String) extends MapsToInteger with MapsToDouble
  case class Reserved(reservedWord: ReservedWord) extends MapsToInteger with MapsToDouble
}
