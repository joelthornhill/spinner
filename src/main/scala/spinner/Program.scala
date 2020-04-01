package spinner
import cats.effect.Resource
import cats.effect.Sync
import spinner.Instruction.Instruction
import org.andrewkilpatrick.elmGen.simulator.SpinSimulator
import cats.implicits._
import spinner.ParserCombinator.DoubleValue
import spinner.ParserCombinator.InstructionValue
import spinner.ParserCombinator.StringValue
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
    instructions: List[(Instruction[F], Int)],
    lines: List[(String, Int)]
  ): F[List[Unit]] = {
    instructions
      .zip(lines)
      .map {
        case (instruction, spin) =>
          val a = instruction._1.spinInstruction().replaceAll(" ", "").trim
          val b = spin._1.replaceAll("\t", "").replaceAll(" ", "").trim

          if (a != b) {
            Sync[F].delay(println(s"$a did not equal $b"))
          } else Sync[F].unit
      }
      .sequence

  }

  def printInstructions(instructions: List[Instruction[F]], elmProgram: Instructions[F]): F[List[Unit]] =
    calculateSkip(instructions, elmProgram).map(i => Sync[F].delay(println(i))).sequence

  def printRun(instructions: List[Instruction[F]], elmProgram: Instructions[F]): F[List[Unit]] =
    calculateSkip(instructions, elmProgram).map(_.runString()).sequence

  def calculateSkip(instructions: List[Instruction[F]], program: Instructions[F]): List[Instruction[F]] = {
    instructions.mapWithIndex {
      case (i, j) =>
        i match {
          case program.Skp(flags, StringValue(value)) =>
            val index = instructions.indexWhere {
              case skipLabel: program.SkipLabel => skipLabel.label == value
              case _ => false
            }

            if (index > 0) program.Skp(flags, DoubleValue((index - j - 1).toDouble))
            else i
          case _ => i
        }
    }
  }

  def runInstructions(
    instructions: List[Instruction[F]],
    elmProgram: Instructions[F]
  ): F[List[Unit]] = calculateSkip(instructions, elmProgram).map(_.run()).sequence

  def getLines(s: String): List[String] =
    s.split("\n")
      .toList
      .map(removeComment)
      .filterNot(_.startsWith(";"))
      .filterNot(_.isEmpty)

  def constants(lines: List[(String, Int)]): F[Map[String, InstructionValue]] = Sync[F].delay(
    lines
      .sortBy(_._2)
      .flatMap(line =>
        parse(equParser, line._1) match {
          case Success(matched: Map[String, InstructionValue], _) => Some(matched)
          case Failure(msg, _)                                    => println(s"FAILURE: $msg"); None
          case Error(msg, _)                                      => println(s"ERROR: $msg"); None
        }
      )
      .flatten
      .toMap
  )

  def spinParse(
    lines: List[(String, Int)],
    elmProgram: Instructions[F]
  ): F[List[(Instruction[F], Int)]] = Sync[F].delay(
    lines
      .sortBy(_._2)
      .flatMap(line =>
        parse(parsed(elmProgram), line._1) match {
          case Success(matched, _) => Some((matched, line._2))
          case Failure(msg, _)     => println(s"FAILURE: $msg, $line"); None
          case Error(msg, _)       => println(s"ERROR: $msg, $line"); None
        }
      )
  )

  def fileContents(filePath: String): F[String] =
    Resource
      .fromAutoCloseable(Sync[F].delay(Source.fromFile(filePath)))
      .use(source => Sync[F].delay(source.getLines.mkString("\n")))


  def runSimulator(program: SpinProgram, testWav: String): Resource[F, SpinSimulator] = {
    val acquire: F[SpinSimulator] = Sync[F].delay(new simulator.SpinSimulator(program, testWav, null, 0.5, 0.5, 0.5))
    val release: SpinSimulator => F[Unit] = simulator => Sync[F].delay(simulator.stopSimulator())
    val simulatorResource = Resource.make(acquire)(release)

    for {
      sim <- simulatorResource
      _ = sim.showInteractiveControls()
      _ = sim.showLevelLogger()
      _ = sim.setLoopMode(true)
    } yield sim
  }

  def run(testWav: String, spinPath: String) = {

    for {
      file <- fileContents(spinPath)
      lines = getLines(file).zipWithIndex
      consts <- constants(lines)
      elm = new Instructions[F](consts)
      parsed <- spinParse(lines, elm)
      _ <- printRun(parsed.map(_._1), elm)
//      _ <- printInstructions(parsed.map(_._1), elm)
      _ <- runInstructions(parsed.map(_._1), elm)
      _ <- checkDifference(parsed, lines)
      _ <- runSimulator(elm, testWav).use(s => Sync[F].delay(s.run()))
    } yield ()
  }
}
