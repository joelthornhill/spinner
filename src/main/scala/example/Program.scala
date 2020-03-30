package example
import cats.effect.Resource
import cats.effect.Sync
import example.Instruction.Instruction
import org.andrewkilpatrick.elmGen.simulator.SpinSimulator
import cats.implicits._
import example.ParserCombinator.DoubleValue
import example.ParserCombinator.InstructionValue
import org.andrewkilpatrick.elmGen.ElmProgram

import scala.io.Source

class Program[F[_]: Sync] extends EquParser with SpinParser[F] {

  private def removeComment(line: String) = {

    val index = line.indexOf(";")

    if (index == -1) line
    else line.splitAt(index)._1
  }

  def printVals(m: Map[String, InstructionValue]): F[Unit] = {
    Sync[F].delay(println(m))
//    m.toList.map { value => Sync[F].delay(println(s"val ${value._1} = ${value._2}")) }.sequence
  }

  def checkDifference(instructions: List[(Instruction[F], Int)], lines: List[(String, Int)]): F[List[Unit]] = {
    instructions.zip(lines).toList.map {
      case (instruction, spin) =>
        val a = instruction._1.spinInstruction().replaceAll(" ", "").trim
        val b = spin._1.replaceAll("\t", "").replaceAll(" ", "").trim

        if (a != b) {
          Sync[F].delay(println(s"$a did not equal $b"))
        } else Sync[F].unit
    }.sequence
//    instructions.map { s =>
//      Sync[F].delay(println(s))
//    }.sequence
  }

//
  def printInstructions(instructions: List[Instruction[F]]): F[List[Unit]] =
    instructions.map(i => Sync[F].delay(println(i))).sequence

  def runInstructions(
    instructions: List[Instruction[F]],
    elmProgram: Instructions[F]
  ): F[List[Unit]] = {
    instructions.mapWithIndex {
      case (i, j) =>
        i match {
          case skip: elmProgram.Skp2 =>

              val index: Int = instructions.indexWhere { l =>
                l match {
                  case label: elmProgram.SkipLabel => label.label == skip.point
                  case _ => false
                }
              }
              if (index > 0 ) skip.replace(DoubleValue((index - j).toDouble)).run()
              else i.run()
          case _ => i.run()
        }
    }.sequence
  }

  def getLines(s: String): List[String] =
    s.split("\n")
      .toList
      .map(removeComment)
      .filterNot(_.startsWith(";"))
      .filterNot(_.isEmpty)

  def constants(lines: List[(String, Int)]): F[Map[String, InstructionValue]] = Sync[F].delay(
    lines.sortBy(_._2)
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

  def spinParse(lines: List[(String, Int)], elmProgram: Instructions[F]): F[List[(Instruction[F], Int)]] = Sync[F].delay(
    lines.sortBy(_._2).flatMap(line =>
        parse(parsed(elmProgram), line._1) match {
//          case Success(_: elmProgram.Equ, _) | Success(elmProgram.EOF, _) =>
//            None
          case Success(matched, _) => Some((matched, line._2))
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

  def blah(program: ElmProgram) = {
    (0 until program.getCodeLen-1).toList.mapWithIndex {
      case (_, j) => Sync[F].delay(println(program.getInstruction(j).getInstructionString))
    }.sequence
  }

  def run(testWav: String, spinPath: String) = {

    for {
      file <- fileContents(spinPath)
      lines = getLines(file).zipWithIndex
      consts <- constants(lines)
      elm = new Instructions(consts)
      _ <- printVals(consts)
      parsed <- spinParse(lines, elm)
      _ <- checkDifference(parsed, lines)
      _ <- printInstructions(parsed.map(_._1))
      _ <- runInstructions(parsed.map(_._1), elm)
//      _ <- blah(elm)
      _ = elm.setSamplerate(44100)
      sim <- Sync[F].delay(new SpinSimulator(elm, testWav, null, 0.5, 0.5, 0.5))
      _ = sim.showInteractiveControls()
      _ = sim.showLevelLogger()
      _ = sim.setLoopMode(true)
      _ <- Sync[F].delay(sim.run())
    } yield ()
  }
}
