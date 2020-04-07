package spinner
import cats.effect.Resource
import cats.effect.Sync
import org.andrewkilpatrick.elmGen.simulator.SpinSimulator
import cats.implicits._
import org.andrewkilpatrick.elmGen.SpinProgram
import org.andrewkilpatrick.elmGen.simulator
import spinner.model.DoubleValue
import spinner.model.InstructionValue
import spinner.model.StringValue
import spinner.parsers.EquParser
import spinner.parsers.SpinParser
import spinner.util.Helpers

import scala.io.Source

class Program[F[_]: Sync] extends EquParser with SpinParser {

  private def removeComment(line: String) = {
    val index = line.indexOf(";")

    if (index == -1) line
    else line.splitAt(index)._1
  }

  private def checkDifference(
    instructions: List[Instruction],
    lines: List[String]
  ): F[List[Unit]] =
    instructions
      .map(_.spinInstruction().replaceAll("\\s", ""))
      .zip(lines.map(_.replaceAll("\\s", "")))
      .map {
        case (instruction, spin) if instruction != spin =>
          Sync[F].delay(println(s"$instruction did not equal $spin"))
        case _ => Sync[F].unit
      }
      .sequence

  private def printInstructions(
    instructions: List[Instruction]
  ): F[List[Unit]] =
    calculateSkip(instructions).map(i => Sync[F].delay(println(i))).sequence

  private def printRun(instruction: List[Instruction], instructions: Spin): F[List[Unit]] =
    calculateSkip(instruction).map(_.runString(instructions)).sequence

  private def calculateSkip(
    instructions: List[Instruction]
  ): List[Instruction] =
    instructions.mapWithIndex {
      case (i, j) =>
        i match {
          case Skp(flags, StringValue(value)) =>
            val index = instructions.indexWhere {
              case skipLabel: SkipLabel => skipLabel.label == value
              case _                    => false
            }

            if (index > 0) Skp(flags, DoubleValue((index - j).toDouble))
            else i
          case _ => i
        }
    }

  private def runInstructions(
    instructions: List[Instruction]
  )(implicit consts: Map[String, InstructionValue]) = {
    calculateSkip(instructions).foldLeft(Sync[F].pure(new Spin(consts))) {
      case (acc, curr) =>
        for {
          program <- acc
          update <- Helpers.updateProgram(program, curr)
        } yield update._1
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
      case Success(matched, _) => Sync[F].pure(matched)
      case NoSuccess(reason, _) =>
        Sync[F].raiseError(
          new Exception(
            s"Could not parse: ${linesWithIndex.find(_._2 == index).map(_._1).getOrElse(reason)}"
          )
        )
    }

  private def constants(linesWithIndex: List[(String, Int)]): F[Map[String, InstructionValue]] = {
    linesWithIndex
      .sortBy(_._2)
      .map(line => Sync[F].delay(parse(equParser, line._1)))
      .sequence
      .flatMap(_.mapWithIndex { case (r, i) => handleResult(r, i, linesWithIndex) }.sequence)
      .map(_.flatten.toMap)
  }

  private def spinParse(
    linesWithIndex: List[(String, Int)]
  ): F[List[Instruction]] =
    linesWithIndex
      .sortBy(_._2)
      .map(line => Sync[F].delay(parse(parsed, line._1)))
      .sequence
      .flatMap(_.mapWithIndex { case (r, i) => handleResult(r, i, linesWithIndex) }.sequence)

  private def fileContents(filePath: String): F[String] =
    Resource
      .fromAutoCloseable(Sync[F].delay(Source.fromFile(filePath)))
      .use(source => Sync[F].delay(source.getLines.mkString("\n")))

  private def runSimulator(program: SpinProgram, testWav: String): Resource[F, SpinSimulator] = {
    val acquire: F[SpinSimulator] =
      Sync[F].delay(new simulator.SpinSimulator(program, testWav, null, 0.5, 0.5, 0.5))
    val release: SpinSimulator => F[Unit] = simulator =>
      Sync[F].delay(println("Stopping simulator")) >> Sync[F].delay(simulator.stopSimulator())
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
      _ <- runSimulator(run, testWav).use(s => Sync[F].delay(s.run()))
    } yield ()
}
