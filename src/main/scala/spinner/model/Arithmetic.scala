package spinner.model

sealed trait Arithmetic {
  def spinString: String
}
case class Division(a: InstructionValue, b: InstructionValue) extends Arithmetic {
  override def spinString: String = s"${a.spinString}/${b.spinString}"
}
case class Addition(a: InstructionValue, b: InstructionValue) extends Arithmetic {
  override def spinString: String = s"${a.spinString}+${b.spinString}"
}
case class Minus(value: InstructionValue) extends Arithmetic {
  override def spinString: String = s"-${value.spinString}"
}
case class Multiplication(a: InstructionValue, b: InstructionValue) extends Arithmetic {
  override def spinString: String = s"${a.spinString}*${b.spinString}"
}
case class DelayEnd(value: InstructionValue) extends Arithmetic {
  override def spinString: String = s"${value.spinString}#"
}
case class MidpointDelay(value: InstructionValue) extends Arithmetic {
  override def spinString: String = s"${value.spinString}^"
}
case class Or(value: List[InstructionValue]) extends Arithmetic {
  override def spinString: String = s"${value.map(_.spinString).mkString("|")}"
}

case class Binary(value: StringValue) extends Arithmetic {
  override def spinString: String = s"%${value.spinString}"
}
