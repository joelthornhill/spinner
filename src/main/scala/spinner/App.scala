package spinner
import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp

object App extends IOApp {

  def run(args: List[String]): IO[ExitCode] = {

    val testWav = "/tmp/test.wav"

    args.headOption match {
      case Some(filePath) => new Program[IO]().run(testWav, filePath).as(ExitCode.Success)
      case _              => IO.raiseError(new Exception("Please include path to spin file")).as(ExitCode(2))
    }
  }

}
