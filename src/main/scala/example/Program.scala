package example
import cats.effect.Resource
import cats.effect.Sync
import example.Instruction.Instruction
import org.andrewkilpatrick.elmGen.simulator.SpinSimulator
import cats.implicits._
import example.ParserCombinator.InstructionValue

import scala.io.Source

class Program[F[_]: Sync] extends EquParser with SpinParser[F] {

  private def removeComment(line: String) = {

    val index = line.indexOf(";")

    if (index == -1) line
    else line.splitAt(index)._1
  }

  def printVals(m: Map[String, InstructionValue]): F[List[Unit]] =
    m.toList.map { value => Sync[F].delay(println(s"val ${value._1} = ${value._2}")) }.sequence

  def printInstructions(instructions: List[Instruction[F]]): F[List[Unit]] =
    instructions.map(i => Sync[F].delay(println(i))).sequence

  def runInstructions(
    instructions: List[Instruction[F]]
  ): F[List[Unit]] = instructions.map(i => i.run()).sequence

  def getLines(s: String): List[String] =
    s.split("\n")
      .toList
      .map(removeComment)
      .filterNot(_.startsWith(";"))
      .filterNot(_.isEmpty)

  def constants(s: String): F[Map[String, InstructionValue]] = Sync[F].delay(
    getLines(s)
      .flatMap(line =>
        parse(equParser, line) match {
          case Success(matched: Map[String, InstructionValue], _) => Some(matched)
          case Failure(msg, _)                                    => println(s"FAILURE: $msg"); None
          case Error(msg, _)                                      => println(s"ERROR: $msg"); None
        }
      )
      .flatten
      .toMap
  )

  def spinParse(s: String, elmProgram: Instructions[F]): F[List[Instruction[F]]] = Sync[F].delay(
    getLines(s)
      .flatMap(line =>
        parse(parsed(elmProgram), line) match {
          case Success(_: elmProgram.Equ, _) | Success(elmProgram.EOF, _) =>
            None
          case Success(matched, _) => Some(matched)
          case Failure(msg, _)     => println(s"FAILURE: $msg, $line"); None
          case Error(msg, _)       => println(s"ERROR: $msg, $line"); None
        }
      )
  )

  def fileContents(filePath: String): F[String] = {
    Resource
      .fromAutoCloseable(Sync[F].delay(Source.fromFile(filePath)))
      .use(source => Sync[F].delay(source.getLines.mkString("\n")))
  }

  def run(testWav: String, spinPath: String) = {

    for {
      file <- fileContents(spinPath)
      consts <- constants(file)
      elm = new Instructions(consts)
      _ <- printVals(consts)
      parsed <- spinParse(file, elm)
      _ <- printInstructions(parsed)
      _ <- runInstructions(parsed)
      _ = elm.setSamplerate(44100)
      sim <- Sync[F].delay(new SpinSimulator(elm, testWav, null, 0.5, 0.5, 0.5))
      _ = sim.showInteractiveControls()
      _ = sim.showLevelLogger()
      _ = sim.setLoopMode(true)
      _ <- Sync[F].delay(sim.run())
    } yield ()
  }
}
