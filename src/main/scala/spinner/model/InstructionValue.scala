package spinner.model

import cats.effect.Sync
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
  def run[F[_]: Sync](implicit c: Consts): F[Double]
}
