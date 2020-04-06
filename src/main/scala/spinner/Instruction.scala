package spinner

import cats.effect.Sync
import spinner.util.Helpers.getDouble
import spinner.util.Helpers.getInt
import spinner.util.Helpers.handleAllOffsets
import spinner.util.Helpers.runner
import spinner.util.Helpers.runnerDD
import spinner.util.Helpers.runnerID
import spinner.util.Helpers.runnerIID
import cats.effect.Sync
import cats.syntax.flatMap._
import cats.syntax.functor._
import spinner.model.Addition
import spinner.model.DoubleValue
import spinner.model.InstructionValue
import spinner.model.StringValue
import spinner.model.WithArithmetic

sealed trait Instruction {
  def run[F[_]: Sync](spin: Spin): F[Unit]
  def runString[F[_]: Sync](spin: Spin): F[Unit]
  def spinInstruction(): String = ""
}

case class Rdax(addr: InstructionValue, scale: InstructionValue) extends Instruction {
  def run[F[_]: Sync](spin: Spin): F[Unit] =
    runnerID(addr, scale, spin.readRegister)(spin.consts)
  def runString[F[_]: Sync](spin: Spin): F[Unit] =
    runnerID(addr, scale, (i, d) => println(s"readRegister($i, $d)"))(spin.consts)
  override def toString = s"readRegister(${addr.toString}, $scale)"
  override def spinInstruction(): String = s"rdax ${addr.spinString},${scale.spinString}"
}

case class Ldax(addr: InstructionValue) extends Instruction {
  def run[F[_]: Sync](spin: Spin): F[Unit] =
    runner(addr, spin.loadAccumulator)(spin.consts)
  def runString[F[_]: Sync](spin: Spin): F[Unit] =
    runner(addr, i => println(s"loadAccumulator($i)"))(spin.consts)

  override def toString: String = s"loadAccumulator($addr)"

  override def spinInstruction(): String = s"ldax ${addr.spinString}"
}

case class Rda(addr: InstructionValue, scale: InstructionValue) extends Instruction {
  def run[F[_]: Sync](
    spin: Spin
  ): F[Unit] =
    handleAllOffsets(
      addr,
      scale,
      spin.readDelay(_: String, _: Double, _: Double),
      spin.readDelay(_: String, _: Int, _: Double),
      spin.readDelay(_: Int, _: Double)
    )(spin.consts)

  def runString[F[_]: Sync](spin: Spin): F[Unit] =
    handleAllOffsets(
      addr,
      scale,
      (v, d1, d2) => println(s"""readDelay(\"$v\", $d1, $d2)"""),
      (v, i, d) => println(s"""readDelay(\"$v\", $i, $d)"""),
      (i, d) => println(s"readDelay($i, $d)")
    )(spin.consts)

  override def toString: String = s"readDelay($addr, $scale)"
  override def spinInstruction(): String = s"rda ${addr.spinString}, ${scale.spinString}"
}

case class Wrax(addr: InstructionValue, scale: InstructionValue) extends Instruction {
  def run[F[_]: Sync](spin: Spin): F[Unit] =
    runnerID(addr, scale, spin.writeRegister)(spin.consts)
  def runString[F[_]: Sync](spin: Spin): F[Unit] =
    runnerID(addr, scale, (i, d) => println(s"writeRegister($i, $d)"))(spin.consts)
  override def toString = s"writeRegister($addr, $scale)"
  override def spinInstruction(): String = s"wrax ${addr.spinString}, ${scale.spinString}"
}

case class Maxx(addr: InstructionValue, scale: InstructionValue) extends Instruction {
  def run[F[_]: Sync](spin: Spin): F[Unit] =
    runnerID(addr, scale, spin.maxx)(spin.consts)
  def runString[F[_]: Sync](spin: Spin): F[Unit] =
    runnerID(addr, scale, (i, d) => println(s"maxx($i, $d)"))(spin.consts)
  override def toString = s"maxx($addr, $scale)"
  override def spinInstruction(): String = s"maxx ${addr.spinString}, ${scale.spinString}"
}

case class Wrap(addr: InstructionValue, scale: InstructionValue) extends Instruction {
  def run[F[_]: Sync](
    spin: Spin
  ): F[Unit] =
    handleAllOffsets(
      addr,
      scale,
      (v, d1, d2) => spin.writeAllpass(v, d1, d2),
      (v, i, d) => spin.writeAllpass(v, i, d),
      (i, d) => spin.writeAllpass(i, d)
    )(spin.consts)

  def runString[F[_]: Sync](spin: Spin): F[Unit] =
    handleAllOffsets(
      addr,
      scale,
      (v, d1, d2) => println(s"""writeAllpass(\"$v\", $d1, $d2)"""),
      (v, i, d) => println(s"""writeAllpass(\"$v\", $i, $d)"""),
      (i, d) => println(s"writeAllpass($i, $d)")
    )(spin.consts)
  override def toString: String = s"writeAllpass($addr, $scale)"
  override def spinInstruction(): String = s"wrap ${addr.spinString}, ${scale.spinString}"
}

case class Wra(addr: InstructionValue, scale: InstructionValue) extends Instruction {
  def run[F[_]: Sync](
    spin: Spin
  ): F[Unit] =
    handleAllOffsets(
      addr,
      scale,
      (v, d1, d2) => spin.writeDelay(v, d1, d2),
      (v, i, d) => spin.writeDelay(v, i, d),
      (i, d) => spin.writeDelay(i, d)
    )(spin.consts)

  def runString[F[_]: Sync](spin: Spin): F[Unit] =
    handleAllOffsets(
      addr,
      scale,
      (v, d1, d2) => println(s"""writeDelay(\"$v\", $d1, $d2)"""),
      (v, i, d) => println(s"""writeDelay(\"$v\", $i, $d)"""),
      (i, d) => println(s"writeDelay($i, $d)")
    )(spin.consts)

  override def toString: String = s"writeDelay($addr, $scale)"
  override def spinInstruction(): String = s"wra ${addr.spinString}, ${scale.spinString}"
}

case class Sof(scale: InstructionValue, offset: InstructionValue) extends Instruction {
  def run[F[_]: Sync](spin: Spin): F[Unit] =
    runnerDD(scale, offset, spin.scaleOffset)(spin.consts)
  def runString[F[_]: Sync](spin: Spin): F[Unit] =
    runnerDD(scale, offset, (d1, d2) => println(s"scaleOffset($d1, $d2)"))(spin.consts)
  override def toString: String = s"scaleOffset($scale, $offset)"
  override def spinInstruction(): String = s"sof ${scale.spinString},${offset.spinString}"
}

case class Equ(name: String, value: InstructionValue) extends Instruction {
  def run[F[_]: Sync](spin: Spin): F[Unit] = Sync[F].unit // Do nothing
  def runString[F[_]: Sync](spin: Spin): F[Unit] =
    Sync[F].delay(println(s"val $name = ${value.spinString.toUpperCase}")) // Do nothing
  override def toString = ""
  override def spinInstruction(): String = s"equ $name ${value.spinString}"
}

case class Mem(name: String, value: InstructionValue) extends Instruction {
  def run[F[_]: Sync](
    spin: Spin
  ): F[Unit] =
    getInt(value)(spin.consts).flatMap(size => Sync[F].delay(spin.allocDelayMem(name, size)))
  def runString[F[_]: Sync](spin: Spin) =
    getInt(value)(spin.consts).flatMap(size =>
      Sync[F].delay(println(s"""allocDelayMem("$name", $size)"""))
    )
  override def toString = s"allocDelayMem($name, $value)"
  override def spinInstruction(): String = s"mem $name  ${value.spinString}"
}

case object EOF extends Instruction {
  def run[F[_]: Sync](spin: Spin): F[Unit] = Sync[F].unit
  def runString[F[_]: Sync](spin: Spin) = Sync[F].unit
  override def spinInstruction(): String = ""
}

case class Skp(flags: InstructionValue, nSkip: InstructionValue) extends Instruction {
  def run[F[_]: Sync](
    spin: Spin
  ): F[Unit] =
    for {
      flags <- getInt(flags)(spin.consts)
      run <- nSkip match {
        case StringValue(_) => Sync[F].unit
        case _ =>
          getInt(nSkip)(spin.consts).flatMap(nSkip => Sync[F].delay(spin.skip(flags, nSkip)))
      }
    } yield run

  def runString[F[_]: Sync](spin: Spin) =
    for {
      flags <- getInt(flags)(spin.consts)
      nSkip <- getInt(nSkip)(spin.consts)
      run <- Sync[F].delay(println(s"skip($flags, $nSkip)"))
    } yield run

  override def toString: String = s"skp($flags, $nSkip)"
  override def spinInstruction(): String = s"skp ${flags.spinString},${nSkip.spinString}"
}
//
//  //  case class Skp2(flags: InstructionValue, point: String) extends Instruction[F] {
//  //    def run() = Sync[F].unit // Do nothing
//  //    def runString() = Sync[F].delay(println("Skp2 does nothing"))
//  //    def replace(nSkip: InstructionValue) = Skp(flags, nSkip)
//  //
//  //    override def toString: String = s"skip($flags, $point)"
//  //
//  //    override def spinInstruction(): String = s"skp ${flags.spinString},$point"
//  //  }
//
case class SkipLabel(label: String) extends Instruction {
  def run[F[_]: Sync](spin: Spin): F[Unit] = Sync[F].unit
  def runString[F[_]: Sync](spin: Spin): F[Unit] =
    Sync[F].delay(println("Skip Label does nothing"))

  override def spinInstruction(): String = s"$label:"
}

case object Clr extends Instruction {
  def run[F[_]: Sync](spin: Spin): F[Unit] = Sync[F].delay(spin.clear())
  def runString[F[_]: Sync](spin: Spin): F[Unit] =
    Sync[F].delay(println("clear()"))
  override def toString: String = "clear()"
  override def spinInstruction(): String = "clr"
}

case object Absa extends Instruction {
  def run[F[_]: Sync](spin: Spin): F[Unit] = Sync[F].delay(spin.absa())
  def runString[F[_]: Sync](spin: Spin): F[Unit] =
    Sync[F].delay(println("absa()"))
  override def toString: String = "absa()"
  override def spinInstruction(): String = "absa"
}

case class Exp(scale: InstructionValue, offset: InstructionValue) extends Instruction {
  def run[F[_]: Sync](spin: Spin): F[Unit] =
    runnerDD(scale, offset, spin.exp)(spin.consts)
  def runString[F[_]: Sync](spin: Spin): F[Unit] =
    runnerDD(scale, offset, (d1, d2) => println(s"exp($d1, $d2)"))(spin.consts)
  override def toString: String = s"exp($scale, $offset)"
  override def spinInstruction(): String = s"exp ${scale.spinString},${offset.spinString}"
}

case class Mulx(addr: InstructionValue) extends Instruction {
  def run[F[_]: Sync](spin: Spin): F[Unit] =
    runner(addr, spin.mulx)(spin.consts)
  def runString[F[_]: Sync](spin: Spin): F[Unit] =
    runner(addr, i => println(s"mulx($i)"))(spin.consts)
  override def toString: String = s"mulx($addr)"
  override def spinInstruction(): String = s"mulx ${addr.spinString}"
}

case class Xor(mask: InstructionValue) extends Instruction {
  def run[F[_]: Sync](spin: Spin): F[Unit] =
    runner(mask, spin.xor)(spin.consts)
  def runString[F[_]: Sync](spin: Spin): F[Unit] =
    runner(mask, i => println(s"xor($i)"))(spin.consts)
  override def toString: String = s"xor($mask)"
  override def spinInstruction(): String = s"xor ${mask.spinString}"
}

case class Wldr(lfo: InstructionValue, freq: InstructionValue, amp: InstructionValue)
    extends Instruction {
  def run[F[_]: Sync](spin: Spin): F[Unit] =
    runner(lfo, freq, amp, spin.loadRampLFO)(spin.consts)
  def runString[F[_]: Sync](spin: Spin): F[Unit] =
    runner(lfo, freq, amp, (i1, i2, i3) => println(s"loadRampLFO($i1, $i2, $i3)"))(
      spin.consts
    )
  override def toString: String = s"loadRampLFO($lfo, $freq, $amp)"
  override def spinInstruction(): String =
    s"wldr ${lfo.spinString},${freq.spinString},${amp.spinString}"
}

case class ChoRda(lfo: InstructionValue, flags: InstructionValue, addr: InstructionValue)
    extends Instruction {
  def run[F[_]: Sync](
    spin: Spin
  ) =
    for {
      lfo <- getInt(lfo)(spin.consts)
      flags <- getInt(flags)(spin.consts)
      run <- addr match {
        case WithArithmetic(Addition(StringValue(value), DoubleValue(offset))) =>
          Sync[F].delay(spin.chorusReadDelay(lfo, flags, value, offset.toInt))
        case _ =>
          getInt(addr)(spin.consts).flatMap(addr =>
            Sync[F].delay(spin.chorusReadDelay(lfo, flags, addr))
          )
      }
    } yield run

  def runString[F[_]: Sync](spin: Spin) =
    for {
      lfo <- getInt(lfo)(spin.consts)
      flags <- getInt(flags)(spin.consts)
      run <- addr match {
        case WithArithmetic(Addition(StringValue(value), DoubleValue(offset))) =>
          Sync[F].delay(println(s"chorusReadDelay($lfo, $flags, $value, ${offset.toInt})"))
        case _ =>
          getInt(addr)(spin.consts).flatMap(addr =>
            Sync[F].delay(println(s"chorusReadDelay($lfo, $flags, $addr)"))
          )
      }
    } yield run

  override def toString: String = s"chorusReadDelay($lfo, $flags, $addr)"

  override def spinInstruction(): String =
    s"cho rda, ${lfo.spinString},${flags.spinString},${addr.spinString}"
}

case class ChoSof(lfo: InstructionValue, flags: InstructionValue, offset: InstructionValue)
    extends Instruction {
  def run[F[_]: Sync](spin: Spin): F[Unit] =
    runnerIID(lfo, flags, offset, spin.chorusScaleOffset)(spin.consts)
  def runString[F[_]: Sync](spin: Spin): F[Unit] =
    runnerIID(lfo, flags, offset, (i1, i2, d1) => println(s"chorusScaleOffset($i1, $i2, $d1)"))(
      spin.consts
    )
  override def toString: String = s"chorusScaleOffset($lfo, $flags, $offset)"
  override def spinInstruction(): String =
    s"cho sof,${lfo.spinString},${flags.spinString},${offset.spinString}"
}

case class ChoRdal(lfo: InstructionValue) extends Instruction {
  def run[F[_]: Sync](spin: Spin): F[Unit] =
    runner(lfo, spin.chorusReadValue)(spin.consts)
  def runString[F[_]: Sync](spin: Spin): F[Unit] =
    runner(lfo, i => println(s"chorusReadValue($i)"))(spin.consts)
  override def toString: String = s"chorusReadValue($lfo)"
  override def spinInstruction(): String = s"cho rdal,${lfo.spinString}"
}

case class Wlds(lfo: InstructionValue, freq: InstructionValue, amp: InstructionValue)
    extends Instruction {
  def run[F[_]: Sync](spin: Spin): F[Unit] =
    runner(lfo, freq, amp, spin.loadSinLFO)(spin.consts)
  def runString[F[_]: Sync](spin: Spin): F[Unit] =
    runner(lfo, freq, amp, (i1, i2, i3) => println(s"loadSinLFO($i1, $i2, $i3)"))(
      spin.consts
    )
  override def toString: String = s"loadSinLFO($lfo, $freq, $amp)"
  override def spinInstruction(): String =
    s"wlds ${lfo.spinString},${freq.spinString},${amp.spinString}"
}

case class Rdfx(addr: InstructionValue, scale: InstructionValue) extends Instruction {
  def run[F[_]: Sync](spin: Spin): F[Unit] =
    runnerID(addr, scale, spin.readRegisterFilter)(spin.consts)
  def runString[F[_]: Sync](spin: Spin): F[Unit] =
    runnerID(addr, scale, (i, d) => println(s"readRegisterFilter($i, $d)"))(spin.consts)
  override def toString: String = s"readRegisterFilter($addr, $scale)"
  override def spinInstruction(): String = s"rdfx ${addr.spinString}, ${scale.spinString}"
}

case class Rmpa(scale: InstructionValue) extends Instruction {
  def run[F[_]: Sync](spin: Spin) =
    getDouble(scale, spin.consts).flatMap(scale => Sync[F].delay(spin.readDelayPointer(scale)))
  def runString[F[_]: Sync](spin: Spin): F[Unit] =
    getDouble(scale, spin.consts).flatMap(scale =>
      Sync[F].delay(println(s"readDelayPointer($scale)"))
    )

  override def toString: String = s"readDelayPointer($scale)"
  override def spinInstruction(): String = s"rmpa ${scale.spinString}"
}

case class Wrlx(addr: InstructionValue, scale: InstructionValue) extends Instruction {
  def run[F[_]: Sync](spin: Spin): F[Unit] =
    runnerID(addr, scale, spin.writeRegisterLowshelf)(spin.consts)
  def runString[F[_]: Sync](spin: Spin): F[Unit] =
    runnerID(addr, scale, (i, d) => println(s"writeRegisterLowshelf($i, $d)"))(
      spin.consts
    )
  override def toString: String = s"writeRegisterLowshelf($addr, $scale)"
  override def spinInstruction(): String = s"wrlx ${addr.spinString},${scale.spinString}"
}

case class Wrhx(addr: InstructionValue, scale: InstructionValue) extends Instruction {
  def run[F[_]: Sync](spin: Spin): F[Unit] =
    runnerID(addr, scale, spin.writeRegisterHighshelf)(spin.consts)
  def runString[F[_]: Sync](spin: Spin): F[Unit] =
    runnerID(addr, scale, (i, d) => println(s"writeRegisterHighshelf($i, $d)"))(
      spin.consts
    )
  override def toString: String = s"writeRegisterHighshelf($addr, $scale)"
  override def spinInstruction(): String = s"wrhx ${addr.spinString}, ${scale.spinString}"
}

case class Log(scale: InstructionValue, offset: InstructionValue) extends Instruction {
  def run[F[_]: Sync](spin: Spin): F[Unit] =
    runnerDD(scale, offset, spin.log)(spin.consts)
  def runString[F[_]: Sync](spin: Spin): F[Unit] =
    runnerDD(scale, offset, (d1, d2) => println(s"log($d1, $d2)"))(spin.consts)
  override def toString: String = s"log($scale, $offset)"
  override def spinInstruction(): String = s"log ${scale.spinString},${offset.spinString}"
}
//
case class And(mask: InstructionValue) extends Instruction {
  def run[F[_]: Sync](spin: Spin): F[Unit] =
    runner(mask, spin.and)(spin.consts)
  def runString[F[_]: Sync](spin: Spin): F[Unit] =
    runner(mask, i => println(s"and($i)"))(spin.consts)
  override def toString: String = s"and($mask)"
  override def spinInstruction(): String = s"and ${mask.spinString}"
}

case class OrInst(mask: InstructionValue) extends Instruction {
  def run[F[_]: Sync](spin: Spin): F[Unit] =
    runner(mask, spin.or)(spin.consts)
  def runString[F[_]: Sync](spin: Spin): F[Unit] =
    runner(mask, i => println(s"or($i)"))(spin.consts)
  override def toString: String = s"or($mask)"
  override def spinInstruction(): String = s"or ${mask.spinString}"
}

case object Loop extends Instruction {
  def run[F[_]: Sync](spin: Spin) = Sync[F].unit
  def runString[F[_]: Sync](spin: Spin) =
    Sync[F].delay(println("loop does nothing"))

  override def spinInstruction(): String = s"loop"
}
