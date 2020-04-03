package spinner
import cats.effect.Resource
import cats.effect.Sync
import spinner.Instruction.Instruction
import spinner.Instruction.SkipLabel
import spinner.Instruction.Skp
import org.andrewkilpatrick.elmGen.simulator.SpinSimulator
import cats.implicits._
import spinner.ParserCombinator.DoubleValue
import spinner.ParserCombinator.InstructionValue
import spinner.ParserCombinator.StringValue
import org.andrewkilpatrick.elmGen.ElmProgram
import org.andrewkilpatrick.elmGen.SpinProgram
import org.andrewkilpatrick.elmGen.simulator

import scala.io.Source

class Program[F[_]: Sync] extends EquParser with SpinParser[F] {

  private def removeComment(line: String) = {
    val index = line.indexOf(";")

    if (index == -1) line
    else line.splitAt(index)._1
  }

  def checkDifference(
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

  def printInstructions(
    instructions: List[Instruction]
  ): F[List[Unit]] =
    calculateSkip(instructions).map(i => Sync[F].delay(println(i))).sequence

  def printRun(instruction: List[Instruction], instructions: Instructions): F[List[Unit]] =
    calculateSkip(instruction).map(_.runString(instructions)).sequence

  def calculateSkip(
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

            if (index > 0) Skp(flags, DoubleValue((index - j - 1).toDouble))
            else i
          case _ => i
        }
    }

//  def printRun(instructions: List[Instruction], elmProgram: Instructions): F[List[Unit]] = {
//    val x: F[List[(Instructions, Unit)]] = calculateSkip(instructions).map { i =>
//      Instruction.updateProgram(elmProgram, i)
//    }.sequence
//
//    x.flatMap(_.run)
//  }

  def runInstructions(
    instructions: List[Instruction]
  )(implicit consts: Map[String, InstructionValue]) = {
    calculateSkip(instructions).foldLeft(Sync[F].pure(new Instructions(consts))) {
      case (acc, curr) =>
        for {
          program <- acc
          update <- Instruction.updateProgram(program, curr)
        } yield update._1
    }
//    calculateSkip(instructions).map(a => Instruction.updateProgram(elmProgram, a)).sequence
//    calculateSkip(instructions).map(a => Instruction.updateProgram(elmProgram, a)).sequence.map(a => a.head._1)
  }

  def getLines(s: String): List[String] =
    s.split("\n")
      .toList
      .map(removeComment)
      .filterNot(_.startsWith(";"))
      .filterNot(_.isEmpty)

  def handleResult[T](
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

  def constants(linesWithIndex: List[(String, Int)]): F[Map[String, InstructionValue]] = {
    linesWithIndex
      .sortBy(_._2)
      .map(line => Sync[F].delay(parse(equParser, line._1)))
      .sequence
      .flatMap(_.mapWithIndex { case (r, i) => handleResult(r, i, linesWithIndex) }.sequence)
      .map(_.flatten.toMap)
  }

  def spinParse(
    linesWithIndex: List[(String, Int)],
    program: Instructions
  ): F[List[Instruction]] =
    linesWithIndex
      .sortBy(_._2)
      .map(line => Sync[F].delay(parse(parsed(program), line._1)))
      .sequence
      .flatMap(_.mapWithIndex { case (r, i) => handleResult(r, i, linesWithIndex) }.sequence)

  def fileContents(filePath: String): F[String] =
    Resource
      .fromAutoCloseable(Sync[F].delay(Source.fromFile(filePath)))
      .use(source => Sync[F].delay(source.getLines.mkString("\n")))

  def runSimulator(program: SpinProgram, testWav: String): Resource[F, SpinSimulator] = {
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

  def run(testWav: String, spinPath: String) =
    for {
      file <- fileContents(spinPath)
      lines = getLines(file).zipWithIndex
      consts <- constants(lines)
      program = new Instructions(consts)
      parsed <- spinParse(lines, program)
      _ <- printRun(parsed, program)
      //      _ <- printInstructions(parsed.map(_._1), elm)
      run <- runInstructions(parsed)(consts)
//      _ <- checkDifference(parsed, lines.map(_._1))
      _ <- runSimulator(run, testWav).use(s => Sync[F].delay(s.run()))
    } yield ()
}
