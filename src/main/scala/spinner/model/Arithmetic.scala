package spinner.model

import cats.MonadError
import spinner.Instruction.Consts
import spinner.util.Helpers._
import cats.syntax.functor._
import cats.syntax.flatMap._
import cats.syntax.applicativeError._

case class Division(a: InstructionValue, b: InstructionValue) extends Arithmetic {
  override def spinString: String = s"${a.spinString}/${b.spinString}"

  override def run[F[_]](implicit c: Consts, M: MonadError[F, Throwable]): F[Double] = {
    for {
      a <- getDouble(a)
      b <- getDouble(b)
    } yield a / b
  }
}

case class Addition(a: InstructionValue, b: InstructionValue) extends Arithmetic {
  override def spinString: String = s"${a.spinString}+${b.spinString}"

  override def run[F[_]](implicit c: Consts, M: MonadError[F, Throwable]): F[Double] = {
    for {
      a <- getDouble(a)
      b <- getDouble(b)
    } yield a + b
  }
}

case class MinusValues(a: InstructionValue, b: InstructionValue) extends Arithmetic {
  override def spinString: String = s"${a.spinString}-${b.spinString}"

  override def run[F[_]](implicit c: Consts, M: MonadError[F, Throwable]): F[Double] = {
    for {
      a <- getDouble(a)
      b <- getDouble(b)
    } yield a - b
  }
}

case class Minus(value: InstructionValue) extends Arithmetic {
  override def spinString: String = s"-${value.spinString}"

  override def run[F[_]](implicit c: Consts, M: MonadError[F, Throwable]): F[Double] =
    getDouble(value).map(-_)
}
case class Multiplication(a: InstructionValue, b: InstructionValue) extends Arithmetic {
  override def spinString: String = s"${a.spinString}*${b.spinString}"

  def run[F[_]](implicit c: Consts, M: MonadError[F, Throwable]): F[Double] = {
    for {
      a <- getDouble(a)
      b <- getDouble(b)
    } yield a * b
  }
}
case class DelayEnd(value: InstructionValue) extends Arithmetic {
  override def spinString: String = s"${value.spinString}#"

  override def run[F[_]](implicit c: Consts, M: MonadError[F, Throwable]): F[Double] =
    M.raiseError(SpinError("Delay end should be handled elsewhere"))
}

case class MidpointDelay(value: StringValue) extends Arithmetic {
  override def spinString: String = s"${value.spinString}^"

  override def run[F[_]](implicit c: Consts, M: MonadError[F, Throwable]): F[Double] =
    M.raiseError(SpinError("Midpoint should be handled elsewhere"))
}
case class Or(value: List[StringValue]) extends Arithmetic {
  override def spinString: String = s"${value.map(_.spinString).mkString("|")}"

  def run[F[_]](implicit c: Consts, M: MonadError[F, Throwable]): F[Double] =
    value.foldLeft(M.pure(0.0)) {
      case (acc, b) =>
        for {
          left <- acc
          right <- findInReserved(b.value).handleErrorWith(_ => findInConsts(b.value))
          asDouble <- M.catchNonFatal((left.toInt | right.toInt).toDouble)
        } yield asDouble
    }
}

case class Binary(value: StringValue) extends Arithmetic {
  override def spinString: String = s"%${value.spinString}"

  override def run[F[_]](implicit c: Consts, M: MonadError[F, Throwable]): F[Double] =
    M.catchNonFatal(Integer.parseInt(value.value.replaceAll("_", ""), 2).toDouble)
}

case class Hex(value: StringValue) extends Arithmetic {
  override def spinString: String = "$" + value.spinString

  override def run[F[_]](implicit c: Consts, M: MonadError[F, Throwable]): F[Double] =
    M.catchNonFatal(Integer.parseInt(value.value, 16).toDouble)
}
