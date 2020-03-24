package example
import cats.effect.{ExitCode, IO, IOApp}
import example.EquParser.EquValue
import example.Instruction.Instruction
import example.ParserCombinator._
import cats.implicits._
import org.andrewkilpatrick.elmGen.simulator.SpinSimulator

import scala.util.Try
import scala.util.parsing.combinator._

trait SpinParser extends RegexParsers {

  def parsed(instructions: Instructions): Parser[Instruction] = {

    val wordRegex = """[a-z0-9-#_]+""".r
    val integerRegex = """(-?[0-9]{1,10})""".r
    val doubleRegex = """-?[0-9]{1,10}([.][0-9]{0,10})?""".r

    def word: Parser[String] = wordRegex ^^ { _.toString }
    def instruction: Parser[StringValue] = wordRegex ^^ { a =>
      StringValue(a.toString)
    }
    def doubleInstruction: Parser[DoubleValue] =
      doubleRegex ^^ (d => DoubleValue(d.toDouble))
    def integerInstruction: Parser[MapsToInteger] = integerRegex ^^ { a =>
      IntegerValue(a.toInt)
    }
    def comma: Parser[String] = ",".r ^^ (_.toString)
    def eof: Parser[Instruction] = """^\s*$""".r ^^ (_ => instructions.EOF)
    def clr: Parser[Instruction] = "clr".r ^^ (_ => instructions.Clr)

    def reservedWords: Parser[Reserved] = """[a-z0-9]+""".r ^? {
      case v if Try(ReservedWord.withName(v.toUpperCase)).isSuccess =>
        Reserved(ReservedWord.withName(v.toUpperCase))
    }

    def rdax: Parser[(MapsToInteger, MapsToDouble) => instructions.Rdax] = "rdax".r ^^ (_ => instructions.Rdax(_, _))
    def rda: Parser[(MapsToInteger, MapsToDouble) => instructions.Rda] = "rda".r ^^ (_ => instructions.Rda(_, _))
    def wrax: Parser[(MapsToInteger, MapsToDouble) => instructions.Wrax] = "wrax".r ^^ (_ => instructions.Wrax(_, _))
    def wrap: Parser[(MapsToInteger, MapsToDouble) => instructions.Wrap] = "wrap".r ^^ (_ => instructions.Wrap(_, _))
    def wra: Parser[(MapsToInteger, MapsToDouble) => instructions.Wra] = "wra".r ^^ (_ => instructions.Wra(_, _))
    def sof: Parser[(MapsToDouble, MapsToDouble) => instructions.Sof] = "sof".r ^^ (_ => instructions.Sof(_, _))
    def exp: Parser[(MapsToDouble, MapsToDouble) => instructions.Exp] = "exp".r ^^ (_ => instructions.Exp(_, _))
    def mem: Parser[(String, MapsToInteger) => instructions.Mem] = "mem".r ^^ (_ => instructions.Mem(_, _))
    def equ: Parser[(String, String) => instructions.Equ] = "equ".r ^^ (_ => instructions.Equ(_, _))
    def skp: Parser[(MapsToInteger, MapsToInteger) => instructions.Skp] = "skp".r ^^ (_ => instructions.Skp(_, _))
    def mulx: Parser[MapsToInteger => instructions.Mulx] = "mulx".r ^^ (_ => instructions.Mulx(_))
    def wldr: Parser[
    (MapsToInteger, MapsToInteger, MapsToInteger) => instructions.Wldr
  ] = "wldr".r ^^ (_ => instructions.Wldr(_, _, _))
    def cho: Parser[String] = "cho".r ^^ (_.toString)
    def crda: Parser[String] = "rda".r ^^ (_.toString)
    def csfo: Parser[String] = "sfo".r ^^ (_.toString)
    def bar: Parser[String] = "|" ^^ (_.toString)
    def barWord: Parser[Option[String ~ String]] = opt(bar ~ word)

    def choRda: Parser[Instruction] =
      cho ~ crda ~ comma ~ reservedOrStringOrInt ~ comma ~ reservedOrStringOrInt ~ barWord ~ comma ~ reservedOrStringOrInt ~ barWord ^^ {
        case _ ~ _ ~ _ ~ lfo ~ _ ~ flags ~ _ ~ _ ~ addr ~ _ =>
          instructions.ChoRda(lfo, flags, addr)
      }
    def choSfo: Parser[Instruction] =
      cho ~ csfo ~ comma ~ reservedOrStringOrInt ~ comma ~ reservedOrStringOrInt ~ barWord ~ comma ~ reservedOrStringOrDouble ~ barWord ^^ {
        case _ ~ _ ~ _ ~ lfo ~ _ ~ flags ~ _ ~ _ ~ offset ~ _ =>
          instructions.ChoSfo(lfo, flags, offset)
      }

    def reservedOrStringOrDouble: Parser[MapsToDouble] =
      reservedWords | doubleInstruction | instruction
    def reservedOrStringOrInt: Parser[MapsToInteger] =
      reservedWords | integerInstruction | instruction

    def equParser: Parser[Instruction] = equ ~ word ~ word ^^ {
      case _ ~ name ~ value => instructions.Equ(name, value)
    }

    def twoDoubles = rdax | rda | wrax | wrap | wra
    def twoInts = sof | exp

    def paramIntParamDouble: Parser[Instruction] =
      twoDoubles ~ reservedOrStringOrInt ~ comma ~ reservedOrStringOrDouble ^^ {
        case instruction ~ int ~ _ ~ double => instruction(int, double)
      }

    def paramDoubleParamDouble: Parser[Instruction] =
      twoInts ~ reservedOrStringOrDouble ~ comma ~ reservedOrStringOrDouble ^^ {
        case instruction ~ d1 ~ _ ~ d2 => instruction(d1, d2)
      }

    def memParser: Parser[Instruction] = mem ~ word ~ integerInstruction ^^ {
      case instruction ~ word ~ integer => instruction(word, integer)
    }

    def skpParser: Parser[Instruction] =
      skp ~ reservedOrStringOrInt ~ comma ~ reservedOrStringOrInt ^^ {
        case instruction ~ flags ~ _ ~ nSkip => instruction(flags, nSkip)
      }

    def mulxParser: Parser[Instruction] = mulx ~ reservedOrStringOrInt ^^ {
      case instruction ~ addr => instruction(addr)
    }

    def wldrParser: Parser[Instruction] =
      wldr ~ reservedOrStringOrInt ~ comma ~ reservedOrStringOrInt ~ comma ~ reservedOrStringOrInt ^^ {
        case instruction ~ lfo ~ _ ~ freq ~ _ ~ amp =>
          instruction(lfo, freq, amp)
      }

    paramIntParamDouble | paramDoubleParamDouble | memParser | equParser | skpParser | clr | mulxParser | wldrParser | choRda | choSfo | eof

  }
}

case object ParserCombinator {

  sealed trait InstructionValue
  sealed trait MapsToInteger extends InstructionValue
  sealed trait MapsToDouble extends InstructionValue

  case class DoubleValue(value: Double) extends MapsToDouble
  case class IntegerValue(value: Int) extends MapsToInteger

  case class StringValue(value: String) extends MapsToInteger with MapsToDouble
  case class Reserved(reservedWord: ReservedWord)
      extends MapsToInteger
      with MapsToDouble
}

//package example
//import cats.effect.{ExitCode, IO, IOApp}
//import example.EquParser.EquValue
//import cats.implicits._
//import example.Instruction.Instruction

object App extends IOApp with EquParser with SpinParser {

  def removeComment(line: String) = {

    val index = line.indexOf(";")

    if (index == -1) line
    else line.splitAt(index)._1
  }

  def printVals(m: Map[String, EquValue]): IO[List[Unit]] =
    m.toList.map { value =>
      IO(println(s"val ${value._1} = ${value._2}"))
    }.sequence

  def printInstructions(instructions: List[Instruction]): IO[List[Unit]] =
    instructions.map(i => IO(println(i))).sequence

  def runInstructions(instructions: List[Instruction],
                      constants: Map[String, EquValue]): IO[List[Unit]] =
    instructions.map(i => IO(i.run(constants))).sequence

  def run(args: List[String]): IO[ExitCode] = {

    val s: String =
      """
        ;sample reverb program for FV-1
        ;minimize number of delays and ops.
        ;4 aps driving 2 AP-delay loops
        ;drive both loop elements, take output from each
        ;no pot controls
        ;output is full reverb, not mixed
        ;22 operations (of 128)

        mem	api1	122
        mem	api2	303
        mem	api3	553
        mem	api4	922

        mem	ap1	3823
        mem	del1	8500	;input = left output

        mem	ap2	4732
        mem	del2	7234	;input = right output

        equ	krt	0.7	;adjust reverb time
        equ	kap	0.625	;adjust AP coefficients
        equ	apout	reg0	;holding reg input AP signal

        ;input all passes (2)

        rdax	adcl,0.25	;read inputs,
        rdax	adcr,0.25	;attenuate, sum and
        rda	api1#,kap	;do 4 APs
        wrap	api1,-kap
        rda	api2#,kap
        wrap	api2,-cap
        rda	api3#,kap
        wrap	api3,-kap
        rda	api4#,kap
        wrap	api4,-kap
        wrax	apout,1		;write to min, keep in ACC

        ;first loop apd:
        			;AP'd input in ACC
        rda	del2#,krt	;read del2, scale by Krt
        rda	ap1#,-kap	;do loop ap
        wrap	ap1,kap
        wra	del1,1.99	;write delay, x2 for dac out
        wrax	dacl,0

        ;second loop apd:

        rdax	apout,1		;get input signal again
        rda	del1#,krt	;as above, to other side of loop
        rda	ap2#,kap
        wrap	ap2,-kap
        wra	del2,1.99
        wrax	dacr,0
      """.stripMargin

    val getLines: List[String] = s
      .split("\n")
      .toList
      .map(removeComment)
      .filterNot(_.startsWith(";"))
      .filterNot(_.isEmpty)

    val constants: IO[Map[String, EquValue]] = IO(
      getLines
        .flatMap(
          line =>
            parse(equParser, line) match {
              case Success(matched: Map[String, EquValue], _) => Some(matched)
              case Failure(msg, _)                            => println(s"FAILURE: $msg"); None
              case Error(msg, _)                              => println(s"ERROR: $msg"); None
          }
        )
        .flatten
        .toMap
    )

    val instructions = new Instructions()

    val spinParse: IO[List[Instruction]] = IO(
      getLines
        .flatMap(
          line =>
            parse(parsed(instructions), line) match {
              case Success(_: instructions.Equ, _) |
                  Success(instructions.EOF, _) =>
                None
              case Success(matched, _) => Some(matched)
              case Failure(msg, _)     => println(s"FAILURE: $msg"); None
              case Error(msg, _)       => println(s"ERROR: $msg"); None
          }
        )
    )

    val testWav = "/tmp/test.wav"

    val program = for {
      consts <- constants
      _ <- printVals(consts)
      parsed <- spinParse
      _ <- printInstructions(parsed)
      _ <- runInstructions(parsed, consts)
      _ <- IO(println("Done"))
      sim <- IO(new SpinSimulator(instructions, testWav, null, 0.5, 0.5, 0.5))
      _ = sim.showInteractiveControls()
      _ = sim.showLevelLogger()
      _ = sim.setLoopMode(true)
    } yield ()


    program.as(ExitCode.Success)

  }
}
