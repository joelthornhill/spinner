package spinner
import spinner.Instruction._
import spinner.ParserCombinator._

import scala.util.matching.Regex
import scala.util.parsing.combinator._

trait CommonParsers extends RegexParsers with Parser {

  val wordRegex: Regex = """[a-zA-Z0-9_]+""".r

  def comma: Parser[String] = ",".r ^^ (_.toString)

  def loop: Parser[InstructionValue] = "loop".r ^^ { a => StringValue(a.toString) }

  def reservedOrStringOrDouble: Parser[InstructionValue] =
    double | word
}

trait SpinParser[F[_]] extends RegexParsers with CommonParsers {

  def parsed(instructions: Instructions): Parser[Instruction] = {

    def eof: Parser[Instruction] = """^\s*$""".r ^^ (_ => EOF)

    // Instructions
    // TODO: xor, jam, nop, not
    def rdax: Parser[(InstructionValue, InstructionValue) => Rdax] =
      "rdax".r ^^ (_ => Rdax)

    def rda: Parser[(InstructionValue, InstructionValue) => Rda] =
      "rda".r ^^ (_ => Rda)

    def wra: Parser[(InstructionValue, InstructionValue) => Wra] =
      "wra".r ^^ (_ => Wra)

    def wrap = "wrap".r ~ reservedOrStringOrDouble ~ comma ~ reservedOrStringOrDouble ^^ {
      case _ ~ addr ~ _ ~ scale => Wrap(addr, scale)
    }

    def wrax: Parser[(InstructionValue, InstructionValue) => Wrax] =
      "wrax".r ^^ (_ => Wrax)
    def sof: Parser[(InstructionValue, InstructionValue) => Sof] =
      "sof".r ^^ (_ => Sof)
    def exp: Parser[(InstructionValue, InstructionValue) => Exp] =
      "exp".r ^^ (_ => Exp)
    def rdfx: Parser[(InstructionValue, InstructionValue) => Rdfx] =
      "rdfx".r ^^ (_ => Rdfx)

    def rmpa: Parser[Instruction] = "rmpa".r ~ reservedOrStringOrDouble ^^ {
      case _ ~ lfo => Rmpa(lfo)
    }

    def maxx: Parser[(InstructionValue, InstructionValue) => Maxx] = "maxx".r ^^ (_ => Maxx)

    def log: Parser[(InstructionValue, InstructionValue) => Log] =
      "log".r ^^ (_ => Log)

    def memParser: Parser[Instruction] = "mem".r ~ wordRegex ~ reservedOrStringOrDouble ^^ {
      case _ ~ word ~ integer => Mem(word, integer)
    }

    def equParser: Parser[Instruction] = "equ".r ~ wordRegex ~ reservedOrStringOrDouble ^^ {
      case _ ~ name ~ value => Equ(name, value)
    }

    def skpParser: Parser[Instruction] =
      "skp".r ~ reservedOrStringOrDouble ~ comma ~ reservedOrStringOrDouble ^^ {
        case _ ~ flags ~ _ ~ nSkip => Skp(flags, nSkip)
      }

//    def skpParser2: Parser[Instruction[F]] =
//      "skp".r ~ reservedOrStringOrDouble ~ comma ~ wordRegex ^^ {
//        case _ ~ flags ~ _ ~ nSkip => instructions.Skp2(flags, nSkip)
//      }

    def mulxParser: Parser[Instruction] = "mulx".r ~ reservedOrStringOrDouble ^^ {
      case _ ~ addr => Mulx(addr)
    }

    def wlds: Parser[(InstructionValue, InstructionValue, InstructionValue) => Wlds] =
      "wlds".r ^^ (_ => Wlds)

    def wldr: Parser[(InstructionValue, InstructionValue, InstructionValue) => Wldr] =
      "wldr".r ^^ (_ => Wldr)

    def wrlx: Parser[(InstructionValue, InstructionValue) => Wrlx] =
      "wrlx".r ^^ (_ => Wrlx)

    def wrhx: Parser[(InstructionValue, InstructionValue) => Wrhx] =
      "wrhx".r ^^ (_ => Wrhx)

    def andParser: Parser[Instruction] = "and".r ~ reservedOrStringOrDouble ^^ {
      case _ ~ mask => And(mask)
    }

    def orParser: Parser[Instruction] = "or".r ~ reservedOrStringOrDouble ^^ {
      case _ ~ mask => Instruction.Or(mask)
    }

    def clr: Parser[Instruction] = "clr".r ^^ (_ => Clr)

    def absa: Parser[Instruction] = "absa".r ^^ (_ => Absa)

    def choRda: Parser[Instruction] =
      "cho".r ~ "rda".r ~ comma ~ opt(reservedOrStringOrDouble) ~ comma ~ opt(
        reservedOrStringOrDouble
      ) ~ comma ~ reservedOrStringOrDouble ^^ {
        case _ ~ _ ~ _ ~ lfo ~ _ ~ flags ~ _ ~ addr =>
          ChoRda(
            lfo.getOrElse(DoubleValue(0.0)),
            flags.getOrElse(DoubleValue(0.0)),
            addr
          )
      }

    def choSfo: Parser[Instruction] =
      "cho".r ~ "sof".r ~ comma ~ reservedOrStringOrDouble ~ comma ~ reservedOrStringOrDouble ~ comma ~ reservedOrStringOrDouble ^^ {
        case _ ~ _ ~ _ ~ lfo ~ _ ~ flags ~ _ ~ offset =>
          ChoSof(lfo, flags, offset)
      }

    def choRdal: Parser[Instruction] =
      "cho".r ~ "rdal".r ~ comma ~ reservedOrStringOrDouble ^^ {
        case _ ~ _ ~ _ ~ lfo => ChoRdal(lfo)
      }

    def choParser = choRda | choSfo | choRdal

    def paramDoubleParamDouble: Parser[Instruction] =
      (rdax | wrax | rdfx | wrhx | wrlx | sof | exp | log | rda | wra | maxx) ~ reservedOrStringOrDouble ~ comma ~ reservedOrStringOrDouble ^^ {
        case instruction ~ d1 ~ _ ~ d2 => instruction(d1, d2)
      }

    def paramDoubleDoubleDouble: Parser[Instruction] =
      (wlds | wldr) ~ reservedOrStringOrDouble ~ comma ~ reservedOrStringOrDouble ~ comma ~ reservedOrStringOrDouble ^^ {
        case instruction ~ lfo ~ _ ~ freq ~ _ ~ amp =>
          instruction(lfo, freq, amp)
      }

//    def loopParser: Parser[Instruction[F]] = "loop".r ^^ (_ => instructions.Loop)

    def loopLabel: Parser[Instruction] =
      ("endclr".r | "endset".r | "loop") ^^ SkipLabel

    paramDoubleParamDouble | memParser | wrap | paramDoubleDoubleDouble | equParser | skpParser | clr | mulxParser | andParser | orParser | choParser | rmpa | absa | loopLabel | eof

  }
}

object ParserCombinator {

  sealed trait InstructionValue {
    def spinString: String
  }

  case class DoubleValue(value: Double) extends InstructionValue {
    override def spinString: String = {
      val asInt = value.toInt
      if (value == asInt) asInt.toString
      else value.toString
    }
  }
  case class StringValue(value: String) extends InstructionValue {
    override def spinString: String = value
  }
  case class WithArithmetic(value: Arithmetic) extends InstructionValue {
    override def spinString: String = value.spinString
  }

  sealed trait Arithmetic {
    def spinString: String
  }
  case class Division(a: InstructionValue, b: InstructionValue) extends Arithmetic {
    override def spinString: String = s"${a.spinString}/${b.spinString}"
  }
  case class Addition(a: InstructionValue, b: InstructionValue) extends Arithmetic {
    override def spinString: String = s"${a.spinString}+${b.spinString}"
  }
  case class Minus(value: InstructionValue) extends Arithmetic {
    override def spinString: String = s"-${value.spinString}"
  }
  case class Multiplication(a: InstructionValue, b: InstructionValue) extends Arithmetic {
    override def spinString: String = s"${a.spinString}*${b.spinString}"
  }
  case class DelayEnd(value: InstructionValue) extends Arithmetic {
    override def spinString: String = s"${value.spinString}#"
  }
  case class MidpointDelay(value: InstructionValue) extends Arithmetic {
    override def spinString: String = s"${value.spinString}^"
  }
  case class Or(value: List[InstructionValue]) extends Arithmetic {
    override def spinString: String = s"${value.map(_.spinString).mkString("|")}"
  }
}
