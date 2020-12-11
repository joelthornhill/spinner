package spinner
import cats.effect.Resource
import cats.effect.Sync
import org.andrewkilpatrick.elmGen.simulator.SpinSimulator
import cats.implicits._
import org.andrewkilpatrick.elmGen.SpinProgram
import org.andrewkilpatrick.elmGen.simulator
import spinner.Instruction.Consts
import spinner.model.DoubleValue
import spinner.model.StringValue
import spinner.parsers.EquParser
import spinner.parsers.SpinParser
import spinner.util.Helpers
import spinner.Params._

import scala.io.Source

class Program[F[_]]()(implicit M: Sync[F]) extends SpinParser[F] with EquParser {

  private def removeComment(line: String) = {
    val index = line.indexOf(";")

    if (index == -1) line
    else line.splitAt(index)._1
  }

  private def checkDifference(
    instructions: List[Instruction[F]],
    lines: List[String]
  ): F[List[Unit]] =
    instructions
      .map(_.spinInstruction().replaceAll("\\s", ""))
      .zip(lines.map(_.replaceAll("\\s", "")))
      .map {
        case (instruction, spin) if instruction != spin =>
          M.delay(println(s"$instruction did not equal $spin"))
        case _ => M.unit
      }
      .sequence

  private def printInstructions(
    instructions: List[Instruction[F]]
  ): F[List[Unit]] =
    calculateSkip(instructions).map(i => M.delay(println(i))).sequence

  private def printRun(instruction: List[Instruction[F]], instructions: Spin): F[List[Unit]] = {
    implicit val c = instructions.consts
    calculateSkip(instruction).map(_.runString(instructions)).sequence
  }

  private def calculateSkip(
    instructions: List[Instruction[F]]
  ): List[Instruction[F]] =
    instructions.mapWithIndex {
      case (i, j) =>
        i match {
          case Skp(flags, NSkip(StringValue(value))) =>
            val index = instructions.indexWhere {
              case skipLabel: SkipLabel[F] => skipLabel.label == value
              case _                       => false
            }

            if (index > 0) Skp(flags, NSkip(DoubleValue((index - j).toDouble)))
            else i
          case _ => i
        }
    }

  private def runInstructions(
    instructions: List[Instruction[F]]
  )(implicit consts: Consts) = {
    calculateSkip(instructions).foldLeft(M.pure(new Spin(consts))) {
      case (acc, curr) =>
        for {
          update <- Helpers.updateProgram(curr).runF
          program <- acc.flatMap(update)
        } yield program._1
    }
  }

  private def getLines(s: String): List[String] =
    s.split("\n")
      .toList
      .map(removeComment)
      .filterNot(_.startsWith(";"))
      .filterNot(_.isEmpty)
      .map(_.toLowerCase())

  private def handleResult[T](
    parseResult: ParseResult[T],
    index: Int,
    linesWithIndex: List[(String, Int)]
  ): F[T] =
    parseResult match {
      case Success(matched, _) => M.pure(matched)
      case NoSuccess(reason, _) =>
        M.raiseError(
          new Exception(
            s"Could not parse: ${linesWithIndex.find(_._2 == index).map(_._1).getOrElse(reason)}"
          )
        )
    }

  private def parseLines[T](
    linesWithIndex: List[(String, Int)],
    parser: Parser[T]
  ): F[List[ParseResult[T]]] =
    linesWithIndex
      .sortBy(_._2)
      .map(line => M.delay(parse(parser, line._1)))
      .sequence

  private def constants(linesWithIndex: List[(String, Int)]): F[Consts] =
    parseLines(linesWithIndex, equParser)
      .flatMap(_.mapWithIndex { case (r, i) => handleResult(r, i, linesWithIndex) }.sequence)
      .map(_.flatten.toMap)

  private def spinParse(
    linesWithIndex: List[(String, Int)]
  ): F[List[Instruction[F]]] =
    parseLines(linesWithIndex, parsed)
      .flatMap(_.mapWithIndex { case (r, i) => handleResult(r, i, linesWithIndex) }.sequence)

  private def fileContents(filePath: String): F[String] =
    Resource
      .fromAutoCloseable(M.delay(Source.fromFile(filePath)))
      .use(source => M.delay(source.getLines.mkString("\n")))

  private def runSimulator(program: SpinProgram, testWav: String): Resource[F, SpinSimulator] = {
    val acquire: F[SpinSimulator] =
      M.delay(new simulator.SpinSimulator(program, testWav, null, 0.5, 0.5, 0.5))
    val release: SpinSimulator => F[Unit] = simulator =>
      M.delay(println("Stopping simulator")) >> M.delay(simulator.stopSimulator())
    val simulatorResource = Resource.make(acquire)(release)

    for {
      sim <- simulatorResource
      _ = sim.showInteractiveControls()
      _ = sim.showLevelLogger()
      _ = sim.setLoopMode(true)
    } yield sim
  }

  def run(testWav: String, spinPath: String): F[Unit] =
    for {
      file <- fileContents(spinPath)
      lines = getLines(file).zipWithIndex
      consts <- constants(lines)
      program = new Spin(consts)
      parsed <- spinParse(lines)
      _ <- printRun(parsed, program)
      _ <- printInstructions(parsed)
      run <- runInstructions(parsed)(consts)
      _ <- checkDifference(parsed, lines.map(_._1))
      _ <- runSimulator(run, testWav).use(s => M.delay(s.run()))
    } yield ()
}
