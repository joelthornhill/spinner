package example
import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import cats.effect.Resource
import cats.effect.Sync
import example.EquParser.EquValue
import example.Instruction.Instruction
import example.ParserCombinator._
import cats.implicits._
import org.andrewkilpatrick.elmGen.simulator.SpinSimulator

import scala.io.Source
import scala.util.Try
import scala.util.parsing.combinator._

trait SpinParser extends RegexParsers {

  def parsed(instructions: Instructions): Parser[Instruction] = {

    val wordRegex = """[a-z0-9-_]+""".r
    val integerRegex = """(-?[0-9]{1,10})""".r
    val doubleRegex = """-?[0-9]{1,10}([.][0-9]{0,10})?""".r

    def word: Parser[String] = wordRegex ^^ { _.toString }
    def instruction: Parser[StringValue] = wordRegex ^^ { a => StringValue(a.toString) }
    def doubleInstruction: Parser[DoubleValue] =
      doubleRegex ^^ (d => DoubleValue(d.toDouble))
    def integerInstruction: Parser[MapsToInteger] = integerRegex ^^ { a => IntegerValue(a.toInt) }
    def comma: Parser[String] = ",".r ^^ (_.toString)
    def eof: Parser[Instruction] = """^\s*$""".r ^^ (_ => instructions.EOF)

    def reservedWords: Parser[Reserved] = """[a-z0-9_]+""".r ^? {
      case v if Try(ReservedWord.withName(v.toUpperCase)).isSuccess =>
        Reserved(ReservedWord.withName(v.toUpperCase))
    }

    // Instructions
    // TODO: rmpa, rdfx, wrhx, whlx, maxx, absa, log, and, or, xor, wlds, jam, nop, not
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
    def mem: Parser[(String, MapsToInteger) => instructions.Mem] =
      "mem".r ^^ (_ => instructions.Mem)
    def equ: Parser[(String, String) => instructions.Equ] = "equ".r ^^ (_ => instructions.Equ)
    def skp: Parser[(MapsToInteger, MapsToInteger) => instructions.Skp] =
      "skp".r ^^ (_ => instructions.Skp)
    def mulx: Parser[MapsToInteger => instructions.Mulx] = "mulx".r ^^ (_ => instructions.Mulx)
    def wldr: Parser[(MapsToInteger, MapsToInteger, MapsToInteger) => instructions.Wldr] =
      "wldr".r ^^ (_ => instructions.Wldr)
    def cho: Parser[String] = "cho".r ^^ (_.toString)
    def crda: Parser[String] = "rda".r ^^ (_.toString)
    def csof: Parser[String] = "sof".r ^^ (_.toString)
    def clr: Parser[Instruction] = "clr".r ^^ (_ => instructions.Clr)

    def bar: Parser[String] = "|" ^^ (_.toString)

    // TODO: handle $ correctly
    def hash: Parser[String] = "#" ^^ (_.toString)

    // TODO: Handle Ors correctly
    def barWord: Parser[Option[String ~ String]] = opt(bar ~ word)

    def choRda: Parser[Instruction] =
      cho ~ crda ~ comma ~ reservedOrStringOrInt ~ comma ~ reservedOrStringOrInt ~ barWord ~ comma ~ reservedOrStringOrInt ~ barWord ^^ {
        case _ ~ _ ~ _ ~ lfo ~ _ ~ flags ~ _ ~ _ ~ addr ~ _ =>
          instructions.ChoRda(lfo, flags, addr)
      }
    def choSfo: Parser[Instruction] =
      cho ~ csof ~ comma ~ reservedOrStringOrInt ~ comma ~ reservedOrStringOrInt ~ barWord ~ comma ~ reservedOrStringOrDouble ~ barWord ^^ {
        case _ ~ _ ~ _ ~ lfo ~ _ ~ flags ~ _ ~ _ ~ offset ~ _ =>
          instructions.ChoSof(lfo, flags, offset)
      }

    def reservedOrStringOrDouble: Parser[MapsToDouble] =
      reservedWords | doubleInstruction | instruction
    def reservedOrStringOrInt: Parser[MapsToInteger] =
      reservedWords | integerInstruction | instruction

    def equParser: Parser[Instruction] = equ ~ word ~ word ^^ {
      case _ ~ name ~ value => instructions.Equ(name, value)
    }

    def integerDouble = rdax | wrax
    def doubleDouble = sof | exp
    def stringDoubleDouble = rda | wrap | wra

    def paramIntParamDouble: Parser[Instruction] =
      integerDouble ~ reservedOrStringOrInt ~ comma ~ reservedOrStringOrDouble ^^ {
        case instruction ~ int ~ _ ~ double => instruction(int, double)
      }

    def paramDoubleParamDouble: Parser[Instruction] =
      doubleDouble ~ reservedOrStringOrDouble ~ comma ~ reservedOrStringOrDouble ^^ {
        case instruction ~ d1 ~ _ ~ d2 => instruction(d1, d2)
      }

    def memParser: Parser[Instruction] = mem ~ word ~ integerInstruction ^^ {
      case instruction ~ word ~ integer => instruction(word, integer)
    }

    def paramStringDoubleDouble: Parser[Instruction] =
      stringDoubleDouble ~ word ~ opt(hash) ~ comma ~ reservedOrStringOrDouble ^^ {
        case instruction ~ word ~ hash ~ _ ~ double =>
          instruction(word, hash.map(_ => 1.0).getOrElse(0.0), double)
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

    paramIntParamDouble | paramDoubleParamDouble | memParser | paramStringDoubleDouble | equParser | skpParser | clr | mulxParser | wldrParser | choRda | choSfo | eof

  }
}

case object ParserCombinator {

  sealed trait InstructionValue
  sealed trait MapsToInteger extends InstructionValue
  sealed trait MapsToDouble extends InstructionValue

  case class DoubleValue(value: Double) extends MapsToDouble
  case class IntegerValue(value: Int) extends MapsToInteger

  case class StringValue(value: String) extends MapsToInteger with MapsToDouble
  case class Reserved(reservedWord: ReservedWord) extends MapsToInteger with MapsToDouble
}

object App extends IOApp with EquParser with SpinParser {

  def removeComment(line: String) = {

    val index = line.indexOf(";")

    if (index == -1) line
    else line.splitAt(index)._1
  }

  def printVals[F[_]: Sync](m: Map[String, EquValue]): F[List[Unit]] =
    m.toList.map { value => Sync[F].delay(println(s"val ${value._1} = ${value._2}")) }.sequence

  def printInstructions[F[_]: Sync](instructions: List[Instruction]): F[List[Unit]] =
    instructions.map(i => Sync[F].delay(println(i))).sequence

  def printRunInstructions[F[_]: Sync](
    instructions: List[Instruction],
    m: Map[String, EquValue]
  ): F[List[Unit]] =
    instructions.map(i => Sync[F].delay(println(i.runString(m)))).sequence

  def runInstructions[F[_]: Sync](
    instructions: List[Instruction],
    constants: Map[String, EquValue]
  ): F[List[Unit]] =
    instructions.map(i => Sync[F].delay(i.run(constants))).sequence

  def run(args: List[String]): IO[ExitCode] = {

    def getLines(s: String): List[String] =
      s.split("\n")
        .toList
        .map(removeComment)
        .filterNot(_.startsWith(";"))
        .filterNot(_.isEmpty)

    def constants[F[_]: Sync](s: String): F[Map[String, EquValue]] = Sync[F].delay(
      getLines(s)
        .flatMap(line =>
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

    def spinParse[F[_]: Sync](s: String): F[List[Instruction]] = Sync[F].delay(
      getLines(s)
        .flatMap(line =>
          parse(parsed(instructions), line) match {
            case Success(_: instructions.Equ, _) | Success(instructions.EOF, _) =>
              None
            case Success(matched, _) => Some(matched)
            case Failure(msg, _)     => println(s"FAILURE: $msg"); None
            case Error(msg, _)       => println(s"ERROR: $msg"); None
          }
        )
    )

    val testWav = "/tmp/test.wav"
    val filePath = "/tmp/test.spn"

    def fileContents[F[_]: Sync] =
      Resource
        .fromAutoCloseable(Sync[F].delay(Source.fromFile(filePath)))
        .use(source => Sync[F].delay(source.getLines.mkString("\n")))

    def program[F[_]: Sync] =
      for {
        file <- fileContents
        consts <- constants(file)
        _ <- printVals(consts)
        parsed <- spinParse(file)
        _ <- printInstructions(parsed)
        _ <- runInstructions(parsed, consts)
        _ <- printRunInstructions(parsed, consts)
        _ = instructions.setSamplerate(44100)
        sim <- Sync[F].delay(new SpinSimulator(instructions, testWav, null, 0.5, 0.5, 0.5))
        _ = sim.showInteractiveControls()
        _ = sim.showLevelLogger()
        _ = sim.setLoopMode(true)
        _ <- Sync[F].delay(sim.run())
      } yield ()

    program[IO].as(ExitCode.Success)

  }
}
