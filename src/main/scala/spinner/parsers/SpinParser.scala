package spinner.parsers

import cats.effect.Sync
import spinner.model.DoubleValue
import spinner._
import spinner.Params._

import scala.util.parsing.combinator.RegexParsers

class SpinParser[F[_]: Sync] extends RegexParsers with CommonParser {

  private def eof: Parser[Instruction[F]] = """^\s*$""".r ^^ (_ => EOF[F])

  private def rdax: Parser[(Addr, Scale) => Rdax[F]] =
    "rdax" ^^ (_ => Rdax[F])

  private def rda: Parser[(Addr, Scale) => Rda[F]] =
    "rda" ^^ (_ => Rda[F])

  private def wra: Parser[(Addr, Scale) => Wra[F]] =
    "wra" ^^ (_ => Wra[F])

  private def wrap: Parser[Instruction[F]] = "wrap" ~ StringOrDouble ~ comma ~ StringOrDouble ^^ {
    case _ ~ addr ~ _ ~ scale => Wrap(Addr(addr), Scale(scale))
  }
  private def wrax: Parser[(Addr, Scale) => Wrax[F]] =
    "wrax" ^^ (_ => Wrax[F])
  private def sof: Parser[(Scale, Offset) => Sof[F]] =
    "sof" ^^ (_ => Sof[F])
  private def exp: Parser[(Scale, Offset) => Exp[F]] =
    "exp" ^^ (_ => Exp[F])
  private def rdfx: Parser[(Addr, Scale) => Rdfx[F]] =
    "rdfx" ^^ (_ => Rdfx[F])

  private def rmpa: Parser[Scale => Instruction[F]] = "rmpa" ^^ (_ => Rmpa[F])

  private def maxx: Parser[(Addr, Scale) => Maxx[F]] =
    "maxx" ^^ (_ => Maxx[F])

  private def log: Parser[(Scale, Offset) => Log[F]] =
    "log" ^^ (_ => Log[F])

  private def memParser: Parser[Instruction[F]] = "mem" ~ wordRegex ~ StringOrDouble ^^ {
    case _ ~ word ~ integer => Mem(word, EquValue(integer))
  }

  private def equParser: Parser[Instruction[F]] = "equ" ~ wordRegex ~ StringOrDouble ^^ {
    case _ ~ name ~ value => Equ(name, EquValue(value))
  }

  private def skpParser: Parser[Instruction[F]] =
    "skp" ~ StringOrDouble ~ comma ~ StringOrDouble ^^ {
      case _ ~ flags ~ _ ~ nSkip => Skp(Flags(flags), NSkip(nSkip))
    }

  private def ldax: Parser[Addr => Ldax[F]] = "ldax" ^^ (_ => Ldax[F])

  private def mulxParser: Parser[Addr => Mulx[F]] = "mulx" ^^ (_ => Mulx[F])

  private def wlds: Parser[(Lfo, Freq, Amp) => Wlds[F]] =
    "wlds" ^^ (_ => Wlds[F])

  private def wldr: Parser[(Lfo, Freq, Amp) => Wldr[F]] =
    "wldr" ^^ (_ => Wldr[F])

  private def wrlx: Parser[(Addr, Scale) => Wrlx[F]] =
    "wrlx" ^^ (_ => Wrlx[F])

  private def wrhx: Parser[(Addr, Scale) => Wrhx[F]] =
    "wrhx".r ^^ (_ => Wrhx[F])

  private def and: Parser[Mask => And[F]] = "and" ^^ (_ => And[F])

  private def orParser: Parser[Mask => OrInst[F]] = "or" ^^ (_ => OrInst[F])

  private def clr: Parser[Instruction[F]] = "clr" ^^ (_ => Clr[F])

  private def absa: Parser[Instruction[F]] = "absa" ^^ (_ => Absa[F])

  private def jam: Parser[Lfo => Instruction[F]] = "jam" ^^ (_ => Jam[F])

  private def xor: Parser[Mask => Instruction[F]] = "xor" ^^ (_ => Xor[F])

  private def not: Parser[Instruction[F]] = "not" ^^ (_ => Not[F])

  private def choRda: Parser[Instruction[F]] =
    "cho" ~ "rda" ~ comma ~ opt(StringOrDouble) ~ comma ~ opt(
      StringOrDouble
    ) ~ comma ~ StringOrDouble ^^ {
      case _ ~ _ ~ _ ~ lfo ~ _ ~ flags ~ _ ~ addr =>
        ChoRda(
          Lfo(lfo.getOrElse(DoubleValue(0.0))),
          Flags(flags.getOrElse(DoubleValue(0.0))),
          Addr(addr)
        )
    }

  private def choSfo: Parser[Instruction[F]] =
    "cho" ~ "sof" ~ comma ~ StringOrDouble ~ comma ~ StringOrDouble ~ comma ~ StringOrDouble ^^ {
      case _ ~ _ ~ _ ~ lfo ~ _ ~ flags ~ _ ~ offset =>
        ChoSof(Lfo(lfo), Flags(flags), Offset(offset))
    }

  private def choRdal: Parser[Instruction[F]] =
    "cho" ~ "rdal" ~ comma ~ StringOrDouble ^^ {
      case _ ~ _ ~ _ ~ lfo => ChoRdal(Lfo(lfo))
    }

  private def choParser: Parser[Instruction[F]] = choRda | choSfo | choRdal

  private def paramScaleOffset: Parser[Instruction[F]] =
    (sof | exp | log) ~ StringOrDouble ~ comma ~ StringOrDouble ^^ {
      case instruction ~ s ~ _ ~ o => instruction(Scale(s), Offset(o))
    }

  private def paramAddrScale: Parser[Instruction[F]] =
    (rdax | wrax | rdfx | wrhx | wrlx | rda | wra | maxx) ~ StringOrDouble ~ comma ~ StringOrDouble ^^ {
      case instruction ~ a ~ _ ~ s => instruction(Addr(a), Scale(s))
    }

  private def paramLfoFreqAmp: Parser[Instruction[F]] =
    (wlds | wldr) ~ StringOrDouble ~ comma ~ StringOrDouble ~ comma ~ StringOrDouble ^^ {
      case instruction ~ lfo ~ _ ~ freq ~ _ ~ amp =>
        instruction(Lfo(lfo), Freq(freq), Amp(amp))
    }

  private def addrParser: Parser[Instruction[F]] =
    (ldax | mulxParser) ~ StringOrDouble ^^ {
      case instruction ~ value => instruction(Addr(value))
    }

  private def lfoParser: Parser[Instruction[F]] =
    jam ~ StringOrDouble ^^ {
      case instruction ~ value => instruction(Lfo(value))
    }

  private def scaleParser: Parser[Instruction[F]] =
    rmpa ~ StringOrDouble ^^ {
      case instruction ~ value => instruction(Scale(value))
    }

  private def maskParser: Parser[Instruction[F]] =
    (orParser | and | xor) ~ StringOrDouble ^^ {
      case instruction ~ value => instruction(Mask(value))
    }

  private def loopLabel: Parser[Instruction[F]] =
    wordRegex ~ ":" ^^ {
      case value ~ _ => SkipLabel(value)
    }

  def parsed: Parser[Instruction[F]] = {
    paramAddrScale | paramScaleOffset | maskParser | wrap | lfoParser | scaleParser | addrParser | memParser | paramLfoFreqAmp | equParser | skpParser | clr | choParser | absa | not | loopLabel | eof
  }

}
