package spinner.model

import cats.MonadError
import spinner.Instruction.Consts

sealed trait InstructionValue {
  def spinString: String
}

case class DoubleValue(value: Double) extends InstructionValue {
  override def spinString: String = {
    val asInt = value.toInt
    if (value == asInt) asInt.toString
    else value.toString
  }
}
case class StringValue(value: String) extends InstructionValue {
  override def spinString: String = value
}

trait Arithmetic extends InstructionValue {
  def run[F[_]](implicit c: Consts, M: MonadError[F, Throwable]): F[Double]
}
