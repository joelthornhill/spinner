package example
import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import cats.implicits._

object App extends IOApp {

  def run(args: List[String]): IO[ExitCode] = {

    val testWav = "/tmp/test.wav"
    val filePath = "/tmp/test.spn"

    new Program[IO]().run(testWav, filePath).as(ExitCode.Success)
  }

}
