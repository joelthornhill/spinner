package spinner
import spinner.Instruction.Instruction
import spinner.ParserCombinator._

import scala.util.parsing.combinator._

trait CommonParsers extends RegexParsers with Parser {

  val wordRegex = """[a-zA-Z0-9_]+""".r

  def comma: Parser[String] = ",".r ^^ (_.toString)

  def loop: Parser[InstructionValue] = "loop".r ^^ { a => StringValue(a.toString) }

  def reservedOrStringOrDouble: Parser[InstructionValue] =
    double | word
}

trait SpinParser[F[_]] extends RegexParsers with CommonParsers {

  def parsed(instructions: Instructions[F]): Parser[Instruction[F]] = {

    def eof: Parser[Instruction[F]] = """^\s*$""".r ^^ (_ => instructions.EOF)

    // Instructions
    // TODO: maxx, absa, xor, jam, nop, not
    def rdax: Parser[(InstructionValue, InstructionValue) => instructions.Rdax] =
      "rdax".r ^^ (_ => instructions.Rdax)

    def rda: Parser[(InstructionValue, InstructionValue) => instructions.Rda] =
      "rda".r ^^ (_ => instructions.Rda)

    def wra: Parser[(InstructionValue, InstructionValue) => instructions.Wra] =
      "wra".r ^^ (_ => instructions.Wra)

    def wrap = "wrap".r ~ reservedOrStringOrDouble ~ comma ~ reservedOrStringOrDouble ^^ {
      case _ ~ addr ~ _ ~ scale => instructions.Wrap(addr, scale)
    }

    def wrax: Parser[(InstructionValue, InstructionValue) => instructions.Wrax] =
      "wrax".r ^^ (_ => instructions.Wrax)
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

//    def skpParser2: Parser[Instruction[F]] =
//      "skp".r ~ reservedOrStringOrDouble ~ comma ~ wordRegex ^^ {
//        case _ ~ flags ~ _ ~ nSkip => instructions.Skp2(flags, nSkip)
//      }

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
      "cho".r ~ "rda".r ~ comma ~ opt(reservedOrStringOrDouble) ~ comma ~ opt(
        reservedOrStringOrDouble
      ) ~ comma ~ reservedOrStringOrDouble ^^ {
        case _ ~ _ ~ _ ~ lfo ~ _ ~ flags ~ _ ~ addr =>
          instructions.ChoRda(
            lfo.getOrElse(DoubleValue(0.0)),
            flags.getOrElse(DoubleValue(0.0)),
            addr
          )
      }

    def choSfo: Parser[Instruction[F]] =
      "cho".r ~ "sof".r ~ comma ~ reservedOrStringOrDouble ~ comma ~ reservedOrStringOrDouble ~ comma ~ reservedOrStringOrDouble ^^ {
        case _ ~ _ ~ _ ~ lfo ~ _ ~ flags ~ _ ~ offset =>
          instructions.ChoSof(lfo, flags, offset)
      }

    def choRdal: Parser[Instruction[F]] =
      "cho".r ~ "rdal".r ~ comma ~ reservedOrStringOrDouble ^^ {
        case _ ~ _ ~ _ ~ lfo => instructions.ChoRdal(lfo)
      }

    def choParser = choRda | choSfo | choRdal

    def paramDoubleParamDouble: Parser[Instruction[F]] =
      (rdax | wrax | rdfx | wrhx | wrlx | sof | exp | log | rda | wra) ~ reservedOrStringOrDouble ~ comma ~ reservedOrStringOrDouble ^^ {
        case instruction ~ d1 ~ _ ~ d2 => instruction(d1, d2)
      }

    def paramDoubleDoubleDouble: Parser[Instruction[F]] =
      (wlds | wldr) ~ reservedOrStringOrDouble ~ comma ~ reservedOrStringOrDouble ~ comma ~ reservedOrStringOrDouble ^^ {
        case instruction ~ lfo ~ _ ~ freq ~ _ ~ amp =>
          instruction(lfo, freq, amp)
      }

//    def loopParser: Parser[Instruction[F]] = "loop".r ^^ (_ => instructions.Loop)

    def loopLabel: Parser[Instruction[F]] =
      ("endclr".r | "endset".r | "loop") ^^ instructions.SkipLabel

    paramDoubleParamDouble | memParser | wrap | paramDoubleDoubleDouble | equParser | skpParser | clr | mulxParser | andParser | orParser | choParser | rmpa | loopLabel | eof

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
  case class Minus(a: InstructionValue) extends Arithmetic {
    override def spinString: String = s"-${a.spinString}"
  }
  case class Multiplication(a: InstructionValue, b: InstructionValue) extends Arithmetic {
    override def spinString: String = s"${a.spinString}*${b.spinString}"
  }
  case class DelayEnd(a: InstructionValue) extends Arithmetic {
    override def spinString: String = s"${a.spinString}#"
  }
  case class MidpointDelay(a: InstructionValue) extends Arithmetic {
    override def spinString: String = s"${a.spinString}^"
  }
  case class Or(a: List[InstructionValue]) extends Arithmetic {
    override def spinString: String = s"${a.map(_.spinString).mkString("|")}"
  }
}
