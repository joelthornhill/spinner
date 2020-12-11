package spinner.model

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
case class WithArithmetic[F[_]](value: Arithmetic) extends InstructionValue {
  override def spinString: String = value.spinString
}
