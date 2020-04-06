package spinner.parsers

import spinner.model.DoubleValue
import spinner.model.InstructionValue
import spinner._

import scala.util.parsing.combinator.RegexParsers

trait SpinParser extends RegexParsers with CommonParser {

  private def eof: Parser[Instruction] = """^\s*$""".r ^^ (_ => EOF)

  // Instructions
  // TODO: xor, jam, nop, not
  private def rdax: Parser[(InstructionValue, InstructionValue) => Rdax] =
    "rdax" ^^ (_ => Rdax)

  private def rda: Parser[(InstructionValue, InstructionValue) => Rda] =
    "rda" ^^ (_ => Rda)

  private def wra: Parser[(InstructionValue, InstructionValue) => Wra] =
    "wra" ^^ (_ => Wra)

  private def wrap = "wrap" ~ StringOrDouble ~ comma ~ StringOrDouble ^^ {
    case _ ~ addr ~ _ ~ scale => Wrap(addr, scale)
  }
//    def wrap: Parser[(InstructionValue, InstructionValue) => Wrap] = "wrap".r ^^ (_ => Wrap)

  private def wrax: Parser[(InstructionValue, InstructionValue) => Wrax] =
    "wrax" ^^ (_ => Wrax)
  private def sof: Parser[(InstructionValue, InstructionValue) => Sof] =
    "sof" ^^ (_ => Sof)
  private def exp: Parser[(InstructionValue, InstructionValue) => Exp] =
    "exp" ^^ (_ => Exp)
  private def rdfx: Parser[(InstructionValue, InstructionValue) => Rdfx] =
    "rdfx" ^^ (_ => Rdfx)

  private def rmpa: Parser[InstructionValue => Rmpa] = "rmpa" ^^ (_ => Rmpa)

  private def maxx: Parser[(InstructionValue, InstructionValue) => Maxx] = "maxx" ^^ (_ => Maxx)

  private def log: Parser[(InstructionValue, InstructionValue) => Log] =
    "log" ^^ (_ => Log)

  private def memParser: Parser[Instruction] = "mem" ~ wordRegex ~ StringOrDouble ^^ {
    case _ ~ word ~ integer => Mem(word, integer)
  }

  private def equParser: Parser[Instruction] = "equ" ~ wordRegex ~ StringOrDouble ^^ {
    case _ ~ name ~ value => Equ(name, value)
  }

  private def skpParser: Parser[Instruction] =
    "skp" ~ StringOrDouble ~ comma ~ StringOrDouble ^^ {
      case _ ~ flags ~ _ ~ nSkip => Skp(flags, nSkip)
    }

  private def ldax: Parser[InstructionValue => Ldax] = "ldax" ^^ (_ => Ldax)

//    def skpParser2: Parser[Instruction[F]] =
//      "skp".r ~ reservedOrStringOrDouble ~ comma ~ wordRegex ^^ {
//        case _ ~ flags ~ _ ~ nSkip => instructions.Skp2(flags, nSkip)
//      }

  private def mulxParser: Parser[InstructionValue => Mulx] = "mulx" ^^ (_ => Mulx)

  private def wlds: Parser[(InstructionValue, InstructionValue, InstructionValue) => Wlds] =
    "wlds" ^^ (_ => Wlds)

  private def wldr: Parser[(InstructionValue, InstructionValue, InstructionValue) => Wldr] =
    "wldr" ^^ (_ => Wldr)

  private def wrlx: Parser[(InstructionValue, InstructionValue) => Wrlx] =
    "wrlx" ^^ (_ => Wrlx)

  private def wrhx: Parser[(InstructionValue, InstructionValue) => Wrhx] =
    "wrhx".r ^^ (_ => Wrhx)

  private def and: Parser[InstructionValue => And] = "and" ^^ (_ => And)

  private def orParser: Parser[InstructionValue => OrInst] = "or" ^^ (_ => OrInst)

  private def clr: Parser[Instruction] = "clr" ^^ (_ => Clr)

  private def absa: Parser[Instruction] = "absa" ^^ (_ => Absa)

  private def choRda: Parser[Instruction] =
    "cho" ~ "rda" ~ comma ~ opt(StringOrDouble) ~ comma ~ opt(
      StringOrDouble
    ) ~ comma ~ StringOrDouble ^^ {
      case _ ~ _ ~ _ ~ lfo ~ _ ~ flags ~ _ ~ addr =>
        ChoRda(
          lfo.getOrElse(DoubleValue(0.0)),
          flags.getOrElse(DoubleValue(0.0)),
          addr
        )
    }

  private def choSfo: Parser[Instruction] =
    "cho" ~ "sof" ~ comma ~ StringOrDouble ~ comma ~ StringOrDouble ~ comma ~ StringOrDouble ^^ {
      case _ ~ _ ~ _ ~ lfo ~ _ ~ flags ~ _ ~ offset =>
        ChoSof(lfo, flags, offset)
    }

  private def choRdal: Parser[Instruction] =
    "cho" ~ "rdal" ~ comma ~ StringOrDouble ^^ {
      case _ ~ _ ~ _ ~ lfo => ChoRdal(lfo)
    }

  private def choParser: Parser[Instruction] = choRda | choSfo | choRdal

  private def paramDoubleParamDouble: Parser[Instruction] =
    (rdax | wrax | rdfx | wrhx | wrlx | sof | exp | log | rda | wra | maxx) ~ StringOrDouble ~ comma ~ StringOrDouble ^^ {
      case instruction ~ d1 ~ _ ~ d2 => instruction(d1, d2)
    }

  private def paramDoubleDoubleDouble: Parser[Instruction] =
    (wlds | wldr) ~ StringOrDouble ~ comma ~ StringOrDouble ~ comma ~ StringOrDouble ^^ {
      case instruction ~ lfo ~ _ ~ freq ~ _ ~ amp =>
        instruction(lfo, freq, amp)
    }

  private def singleParser: Parser[Instruction] =
    (orParser | and | mulxParser | ldax | rmpa) ~ StringOrDouble ^^ {
      case instruction ~ value => instruction(value)
    }

//    def loopParser: Parser[Instruction[F]] = "loop".r ^^ (_ => instructions.Loop)

  private def loopLabel: Parser[Instruction] =
    wordRegex ~ ":" ^^ {
      case value ~ _ => SkipLabel(value)
    }

  def parsed: Parser[Instruction] =
    paramDoubleParamDouble | singleParser | memParser | wrap | paramDoubleDoubleDouble | equParser | skpParser | clr | choParser | absa | loopLabel | eof

}
