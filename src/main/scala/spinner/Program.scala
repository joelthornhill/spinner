package spinner
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
import spinner.Params._
import fs2._

import scala.io.Source

class Program[F[_]]()(implicit M: Sync[F]) extends SpinParser[F] with EquParser {

  case class Result(consts: Map[String, InstructionValue], instruction: Instruction[F])

  private def removeComment(line: String) = {
    val index = line.indexOf(";")

    if (index == -1) line
    else line.splitAt(index)._1
  }

  private def checkDifference(
    line: String,
    instruction: Instruction[F]
  ): F[Unit] = {
    val l = line.replaceAll("\\s", "")
    val i = instruction.spinInstruction().replaceAll("\\s", "")

    if (l != i) M.delay(println(s"$i did not equal $l"))
    else M.unit
  }

  private def calcSkip(s: Stream[F, Result]) = {
    val withIndex = s.zipWithIndex

    def findIndex(value: String): Stream[F, Long] =
      withIndex
        .find(_._1.instruction match {
          case skipLabel: SkipLabel[F] => skipLabel.label == value
          case _                       => false
        })
        .map(_._2)

    withIndex.flatMap {
      case (result, j) =>
        result.instruction match {
          case Skp(flags, NSkip(StringValue(value))) =>
            findIndex(value).map { index =>
              if (index > 0)
                result.copy(instruction = Skp(flags, NSkip(DoubleValue((index - j).toDouble))))
              else result
            }
          case _ => Stream.emit(result)
        }
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
    line: String
  ): F[T] =
    parseResult match {
      case Success(matched, _) => M.pure(matched)
      case NoSuccess(reason, _) =>
        M.raiseError(
          new Exception(
            s"Could not parse: $line, $reason"
          )
        )
    }

  private def parseAndHandleResult[T](line: String, parser: Parser[T]): F[T] =
    M.delay(parse(parser, line)).flatMap(parseResult => handleResult(parseResult, line))

  private def fileContents(filePath: String): Stream[F, String] =
    Stream
      .bracket(M.delay(Source.fromFile(filePath)))(s => M.delay(s.close()))
      .map(_.getLines().mkString("\n"))

  private def runSimulator(program: SpinProgram, testWav: String) = {
    val acquire: F[SpinSimulator] =
      M.delay(new simulator.SpinSimulator(program, testWav, null, 0.5, 0.5, 0.5))
    val release: SpinSimulator => F[Unit] = simulator =>
      M.delay(println("Stopping simulator")) >> M.delay(simulator.stopSimulator())

    Stream
      .bracket(acquire)(release)
      .evalTap(sim =>
        M.delay {
          sim.showInteractiveControls()
          sim.showLevelLogger()
          sim.setLoopMode(true)
        }
      )
      .evalMap(s => M.delay(s.run()))
  }

  private def accInstructions(
    results: List[Result]
  ): Stream[F, Spin] = {
    val instructions: Stream[F, Instruction[F]] = Stream.emits(results.map(_.instruction))
    val consts: Map[String, InstructionValue] = results.flatMap(_.consts).toMap

    instructions.evalScan(new Spin(consts))((acc, i) =>
      Helpers.updateProgram(i).runF.flatMap(f => f(acc)).map(_._1)
    )
  }

  private def handleLine(line: String): F[Result] =
    for {
      consts <- parseAndHandleResult(line, equParser)
      parseResult <- parseAndHandleResult(line, parsed)
      _ <- checkDifference(line, parseResult)
      _ <- M.delay(println(parseResult.spinInstruction()))
    } yield Result(consts, parseResult)

  def run(testWav: String, spinPath: String): F[Unit] = {

    val instructions: Stream[F, Result] =
      fileContents(spinPath).flatMap(file => Stream.emits(getLines(file))).evalMap(handleLine)

    calcSkip(instructions).compile.toList.flatMap(
      accInstructions(_)
        .takeRight(1)
        .flatMap(runSimulator(_, testWav))
        .compile
        .drain
    )
  }
}
