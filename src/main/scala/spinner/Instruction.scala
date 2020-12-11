package spinner

import cats.effect.Sync
import spinner.util.Helpers.getDouble
import spinner.util.Helpers.getInt
import spinner.util.Helpers.handleAllOffsets
import spinner.util.Helpers.runner
import cats.effect.Sync
import cats.syntax.flatMap._
import cats.syntax.functor._
import spinner.model.Addition
import spinner.model.DoubleValue
import spinner.model.InstructionValue
import spinner.model.StringValue
import spinner.model.WithArithmetic

sealed trait Instruction[F[_]] {
  def run(spin: Spin): F[Unit]
  def runString(spin: Spin): F[Unit]
  def spinInstruction(): String = ""
}

sealed trait ParamType {
  val value: InstructionValue
  def spinString: String = value.spinString
}
case class Addr(value: InstructionValue) extends ParamType
case class Scale(value: InstructionValue) extends ParamType
case class Offset(value: InstructionValue) extends ParamType
case class Lfo(value: InstructionValue) extends ParamType
case class Freq(value: InstructionValue) extends ParamType
case class Amp(value: InstructionValue) extends ParamType
case class Flags(value: InstructionValue) extends ParamType
case class Mask(value: InstructionValue) extends ParamType
case class EquValue(value: InstructionValue) extends ParamType
case class NSkip(value: InstructionValue) extends ParamType

case class Rdax[F[_]: Sync](addr: Addr, scale: Scale) extends Instruction[F] {
  def run(spin: Spin): F[Unit] =
    runner(addr, scale, spin.readRegister _)(spin.consts)
  def runString(spin: Spin): F[Unit] =
    runner(addr, scale, (i: Int, d: Double) => println(s"readRegister($i, $d)"))(spin.consts)

  override def toString = s"readRegister($addr, $scale)"
  override def spinInstruction(): String =
    s"rdax ${addr.spinString},${scale.spinString}"
}

import cats.syntax.flatMap._

case class Ldax[F[_]: Sync](addr: Addr) extends Instruction[F] {
  def run(spin: Spin): F[Unit] =
    getInt(addr.value)(spin.consts).map(a => spin.loadAccumulator(a))

  def runString(spin: Spin): F[Unit] =
    getInt(addr.value)(spin.consts).map(a => println(s"loadAccumulator($a)"))

  override def toString: String = s"loadAccumulator($addr)"

  override def spinInstruction(): String = s"ldax ${addr.spinString}"
}

case class Rda[F[_]: Sync](addr: Addr, scale: Scale) extends Instruction[F] {
  def run(
    spin: Spin
  ): F[Unit] =
    handleAllOffsets(
      addr,
      scale,
      spin.readDelay(_: String, _: Double, _: Double),
      spin.readDelay(_: String, _: Int, _: Double),
      spin.readDelay(_: Int, _: Double)
    )(spin.consts)

  def runString(spin: Spin): F[Unit] =
    handleAllOffsets(
      addr,
      scale,
      (v, d1, d2) => println(s"""readDelay(\"$v\", $d1, $d2)"""),
      (v, i, d) => println(s"""readDelay(\"$v\", $i, $d)"""),
      (i, d) => println(s"readDelay($i, $d)")
    )(spin.consts)

  override def toString: String = s"readDelay($addr, $scale)"
  override def spinInstruction(): String =
    s"rda ${addr.spinString}, ${scale.spinString}"
}

case class Wrax[F[_]: Sync](addr: Addr, scale: Scale) extends Instruction[F] {
  def run(spin: Spin): F[Unit] =
    for {
      a <- getInt(addr.value)(spin.consts)
      b <- getDouble(scale.value, spin.consts)
    } yield spin.writeRegister(a, b)

  def runString(spin: Spin): F[Unit] =
    for {
      a <- getInt(addr.value)(spin.consts)
      b <- getDouble(scale.value, spin.consts)
    } yield println(s"writeRegister($a, $b)")

  override def toString = s"writeRegister($addr, $scale)"
  override def spinInstruction(): String =
    s"wrax ${addr.spinString}, ${scale.spinString}"
}

case class Maxx[F[_]: Sync](addr: Addr, scale: Scale) extends Instruction[F] {
  def run(spin: Spin): F[Unit] =
    runner(addr, scale, spin.maxx _)(spin.consts)
  def runString(spin: Spin): F[Unit] =
    runner(addr, scale, (i: Int, d: Double) => println(s"maxx($i, $d)"))(spin.consts)
  override def toString = s"maxx($addr, $scale)"
  override def spinInstruction(): String =
    s"maxx ${addr.spinString}, ${scale.spinString}"
}

case class Wrap[F[_]: Sync](addr: Addr, scale: Scale) extends Instruction[F] {
  def run(
    spin: Spin
  ): F[Unit] =
    handleAllOffsets(
      addr,
      scale,
      (v, d1, d2) => spin.writeAllpass(v, d1, d2),
      (v, i, d) => spin.writeAllpass(v, i, d),
      (i, d) => spin.writeAllpass(i, d)
    )(spin.consts)

  def runString(spin: Spin): F[Unit] =
    handleAllOffsets(
      addr,
      scale,
      (v, d1, d2) => println(s"""writeAllpass(\"$v\", $d1, $d2)"""),
      (v, i, d) => println(s"""writeAllpass(\"$v\", $i, $d)"""),
      (i, d) => println(s"writeAllpass($i, $d)")
    )(spin.consts)
  override def toString: String = s"writeAllpass($addr, $scale)"
  override def spinInstruction(): String =
    s"wrap ${addr.spinString}, ${scale.spinString}"
}

case class Wra[F[_]: Sync](addr: Addr, scale: Scale) extends Instruction[F] {
  def run(
    spin: Spin
  ): F[Unit] =
    handleAllOffsets(
      addr,
      scale,
      (v, d1, d2) => spin.writeDelay(v, d1, d2),
      (v, i, d) => spin.writeDelay(v, i, d),
      (i, d) => spin.writeDelay(i, d)
    )(spin.consts)

  def runString(spin: Spin): F[Unit] =
    handleAllOffsets(
      addr,
      scale,
      (v, d1, d2) => println(s"""writeDelay(\"$v\", $d1, $d2)"""),
      (v, i, d) => println(s"""writeDelay(\"$v\", $i, $d)"""),
      (i, d) => println(s"writeDelay($i, $d)")
    )(spin.consts)

  override def toString: String = s"writeDelay($addr, $scale)"
  override def spinInstruction(): String =
    s"wra ${addr.spinString}, ${scale.spinString}"
}

case class Sof[F[_]: Sync](scale: Scale, offset: Offset) extends Instruction[F] {
  def run(spin: Spin): F[Unit] =
    runner(scale, offset, spin.scaleOffset _)(spin.consts)
  def runString(spin: Spin): F[Unit] =
    runner(scale, offset, (d1: Double, d2: Double) => println(s"scaleOffset($d1, $d2)"))(
      spin.consts
    )
  override def toString: String = s"scaleOffset($scale, $offset)"
  override def spinInstruction(): String =
    s"sof ${scale.spinString},${offset.spinString}"
}

case class Equ[F[_]: Sync](name: String, value: EquValue) extends Instruction[F] {
  def run(spin: Spin): F[Unit] = Sync[F].unit // Do nothing
  def runString(spin: Spin): F[Unit] =
    Sync[F].delay(println(s"val $name = ${value.spinString.toUpperCase}")) // Do nothing
  override def toString = ""
  override def spinInstruction(): String = s"equ $name ${value.spinString}"
}

case class Mem[F[_]: Sync](name: String, value: EquValue) extends Instruction[F] {
  def run(
    spin: Spin
  ): F[Unit] =
    getInt(value.value)(spin.consts).flatMap(size => Sync[F].delay(spin.allocDelayMem(name, size)))
  def runString(spin: Spin) =
    getInt(value.value)(spin.consts).flatMap(size =>
      Sync[F].delay(println(s"""allocDelayMem("$name", $size)"""))
    )
  override def toString = s"allocDelayMem($name, $value)"
  override def spinInstruction(): String = s"mem $name  ${value.spinString}"
}

case class EOF[F[_]: Sync]() extends Instruction[F] {
  def run(spin: Spin): F[Unit] = Sync[F].unit
  def runString(spin: Spin) = Sync[F].unit
  override def spinInstruction(): String = ""
}

case class Skp[F[_]: Sync](flags: Flags, nSkip: NSkip) extends Instruction[F] {
  def run(
    spin: Spin
  ): F[Unit] =
    for {
      flags <- getInt(flags.value)(spin.consts)
      run <- nSkip.value match {
        case StringValue(_) => Sync[F].unit
        case _ =>
          getInt(nSkip.value)(spin.consts).flatMap(nSkip => Sync[F].delay(spin.skip(flags, nSkip)))
      }
    } yield run

  def runString(spin: Spin) =
    for {
      flags <- getInt(flags.value)(spin.consts)
      nSkip <- getInt(nSkip.value)(spin.consts)
      run <- Sync[F].delay(println(s"skip($flags, $nSkip)"))
    } yield run

  override def toString: String = s"skp($flags, $nSkip)"
  override def spinInstruction(): String =
    s"skp ${flags.spinString},${nSkip.spinString}"
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
case class SkipLabel[F[_]: Sync](label: String) extends Instruction[F] {
  def run(spin: Spin): F[Unit] = Sync[F].unit
  def runString(spin: Spin): F[Unit] =
    Sync[F].delay(println("SpinProgram"))

  override def spinInstruction(): String = s"$label:"
}

case class Clr[F[_]: Sync]() extends Instruction[F] {
  def run(spin: Spin): F[Unit] = Sync[F].delay(spin.clear())
  def runString(spin: Spin): F[Unit] =
    Sync[F].delay(println("clear()"))
  override def toString: String = "clear()"
  override def spinInstruction(): String = "clr"
}

case class Absa[F[_]: Sync]() extends Instruction[F] {
  def run(spin: Spin): F[Unit] = Sync[F].delay(spin.absa())
  def runString(spin: Spin): F[Unit] =
    Sync[F].delay(println("absa()"))
  override def toString: String = "absa()"
  override def spinInstruction(): String = "absa"
}

case class Exp[F[_]: Sync](scale: Scale, offset: Offset) extends Instruction[F] {
  def run(spin: Spin): F[Unit] =
    runner(scale, offset, spin.exp _)(spin.consts)
  def runString(spin: Spin): F[Unit] =
    runner(scale, offset, (d1: Double, d2: Double) => println(s"exp($d1, $d2)"))(spin.consts)
  override def toString: String = s"exp($scale, $offset)"
  override def spinInstruction(): String =
    s"exp ${scale.spinString},${offset.spinString}"
}

case class Mulx[F[_]: Sync](addr: Addr) extends Instruction[F] {
  def run(spin: Spin): F[Unit] =
    runner(addr, spin.mulx)(spin.consts)
  def runString(spin: Spin): F[Unit] =
    runner(addr, i => println(s"mulx($i)"))(spin.consts)
  override def toString: String = s"mulx($addr)"
  override def spinInstruction(): String = s"mulx ${addr.spinString}"
}

case class Xor[F[_]: Sync](mask: Mask) extends Instruction[F] {
  def run(spin: Spin): F[Unit] =
    runner(mask, spin.xor)(spin.consts)
  def runString(spin: Spin): F[Unit] =
    runner(mask, i => println(s"xor($i)"))(spin.consts)
  override def toString: String = s"xor($mask)"
  override def spinInstruction(): String = s"xor ${mask.spinString}"
}

case class Wldr[F[_]: Sync](lfo: Lfo, freq: Freq, amp: Amp) extends Instruction[F] {
  def run(spin: Spin): F[Unit] =
    runner(lfo, freq, amp, spin.loadRampLFO _)(spin.consts)
  def runString(spin: Spin): F[Unit] =
    runner(lfo, freq, amp, (i1: Int, i2: Int, i3: Int) => println(s"loadRampLFO($i1, $i2, $i3)"))(
      spin.consts
    )

  override def toString: String = s"loadRampLFO($lfo, $freq, $amp)"
  override def spinInstruction(): String =
    s"wldr ${lfo.spinString},${freq.spinString},${amp.spinString}"
}

case class ChoRda[F[_]: Sync](
  lfo: Lfo,
  flags: Flags,
  addr: Addr
) extends Instruction[F] {
  def run(
    spin: Spin
  ) =
    for {
      lfo <- getInt(lfo.value)(spin.consts)
      flags <- getInt(flags.value)(spin.consts)
      run <- addr.value match {
        case WithArithmetic(Addition(StringValue(value), DoubleValue(offset))) =>
          Sync[F].delay(spin.chorusReadDelay(lfo, flags, value, offset.toInt))
        case StringValue(value) =>
          Sync[F].delay(spin.chorusReadDelay(lfo, flags, value, 0))
        case _ =>
          getInt(addr.value)(spin.consts).flatMap(addr =>
            Sync[F].delay(spin.chorusReadDelay(lfo, flags, addr))
          )
      }
    } yield run

  def runString(spin: Spin) =
    for {
      lfo <- getInt(lfo.value)(spin.consts)
      flags <- getInt(flags.value)(spin.consts)
      run <- addr.value match {
        case WithArithmetic(Addition(StringValue(value), DoubleValue(offset))) =>
          Sync[F].delay(println(s"chorusReadDelay($lfo, $flags, $value, ${offset.toInt})"))
        case StringValue(value) =>
          Sync[F].delay(println(s"chorusReadDelay($lfo, $flags, $value, 0)"))
        case _ =>
          getInt(addr.value)(spin.consts).flatMap(addr =>
            Sync[F].delay(println(s"chorusReadDelay($lfo, $flags, $addr)"))
          )
      }
    } yield run

  override def toString: String = s"chorusReadDelay($lfo, $flags, $addr)"

  override def spinInstruction(): String =
    s"cho rda, ${lfo.spinString},${flags.spinString},${addr.spinString}"
}

case class ChoSof[F[_]: Sync](
  lfo: Lfo,
  flags: Flags,
  offset: Offset
) extends Instruction[F] {
  def run(spin: Spin): F[Unit] =
    runner(lfo, flags, offset, spin.chorusScaleOffset _)(spin.consts)
  def runString(spin: Spin): F[Unit] =
    runner(
      lfo,
      flags,
      offset,
      (i1: Int, i2: Int, d1: Double) => println(s"chorusScaleOffset($i1, $i2, $d1)")
    )(
      spin.consts
    )
  override def toString: String = s"chorusScaleOffset($lfo, $flags, $offset)"
  override def spinInstruction(): String =
    s"cho sof,${lfo.spinString},${flags.spinString},${offset.spinString}"
}

case class ChoRdal[F[_]: Sync](lfo: Lfo) extends Instruction[F] {
  def run(spin: Spin): F[Unit] =
    runner(lfo, spin.chorusReadValue)(spin.consts)
  def runString(spin: Spin): F[Unit] =
    runner(lfo, i => println(s"chorusReadValue($i)"))(spin.consts)
  override def toString: String = s"chorusReadValue($lfo)"
  override def spinInstruction(): String = s"cho rdal,${lfo.spinString}"
}

case class Wlds[F[_]: Sync](lfo: Lfo, freq: Freq, amp: Amp) extends Instruction[F] {
  def run(spin: Spin): F[Unit] =
    runner(lfo, freq, amp, spin.loadSinLFO(_: Int, _: Int, _: Int))(spin.consts)
  def runString(spin: Spin): F[Unit] =
    runner(lfo, freq, amp, (i1: Int, i2: Int, i3: Int) => println(s"loadSinLFO($i1, $i2, $i3)"))(
      spin.consts
    )
  override def toString: String = s"loadSinLFO($lfo, $freq, $amp)"
  override def spinInstruction(): String =
    s"wlds ${lfo.spinString},${freq.spinString},${amp.spinString}"
}

case class Rdfx[F[_]: Sync](addr: Addr, scale: Scale) extends Instruction[F] {
  def run(spin: Spin): F[Unit] =
    runner(addr, scale, spin.readRegisterFilter _)(spin.consts)
  def runString(spin: Spin): F[Unit] =
    runner(addr, scale, (i: Int, d: Double) => println(s"readRegisterFilter($i, $d)"))(spin.consts)
  override def toString: String = s"readRegisterFilter($addr, $scale)"
  override def spinInstruction(): String =
    s"rdfx ${addr.spinString}, ${scale.spinString}"
}

case class Rmpa[F[_]: Sync](scale: Scale) extends Instruction[F] {
  def run(spin: Spin) =
    getDouble(scale.value, spin.consts).flatMap(scale =>
      Sync[F].delay(spin.readDelayPointer(scale))
    )
  def runString(spin: Spin): F[Unit] =
    getDouble(scale.value, spin.consts).flatMap(scale =>
      Sync[F].delay(println(s"readDelayPointer($scale)"))
    )

  override def toString: String = s"readDelayPointer($scale)"
  override def spinInstruction(): String = s"rmpa ${scale.spinString}"
}

case class Wrlx[F[_]: Sync](addr: Addr, scale: Scale) extends Instruction[F] {
  def run(spin: Spin): F[Unit] =
    runner(addr, scale, spin.writeRegisterLowshelf _)(spin.consts)
  def runString(spin: Spin): F[Unit] =
    runner(addr, scale, (i: Int, d: Double) => println(s"writeRegisterLowshelf($i, $d)"))(
      spin.consts
    )
  override def toString: String = s"writeRegisterLowshelf($addr, $scale)"
  override def spinInstruction(): String =
    s"wrlx ${addr.spinString},${scale.spinString}"
}

case class Wrhx[F[_]: Sync](addr: Addr, scale: Scale) extends Instruction[F] {
  def run(spin: Spin): F[Unit] =
    runner(addr, scale, spin.writeRegisterHighshelf _)(spin.consts)
  def runString(spin: Spin): F[Unit] =
    runner(addr, scale, (i: Int, d: Double) => println(s"writeRegisterHighshelf($i, $d)"))(
      spin.consts
    )
  override def toString: String = s"writeRegisterHighshelf($addr, $scale)"
  override def spinInstruction(): String =
    s"wrhx ${addr.spinString}, ${scale.spinString}"
}

case class Log[F[_]: Sync](scale: Scale, offset: Offset) extends Instruction[F] {
  def run(spin: Spin): F[Unit] =
    runner(scale, offset, spin.log _)(spin.consts)
  def runString(spin: Spin): F[Unit] =
    runner(scale, offset, (d1: Double, d2: Double) => println(s"log($d1, $d2)"))(spin.consts)
  override def toString: String = s"log($scale, $offset)"
  override def spinInstruction(): String =
    s"log ${scale.spinString},${offset.spinString}"
}

case class And[F[_]: Sync](mask: Mask) extends Instruction[F] {
  def run(spin: Spin): F[Unit] =
    runner(mask, spin.and)(spin.consts)
  def runString(spin: Spin): F[Unit] =
    runner(mask, i => println(s"and($i)"))(spin.consts)
  override def toString: String = s"and($mask)"
  override def spinInstruction(): String = s"and ${mask.spinString}"
}

case class OrInst[F[_]: Sync](mask: Mask) extends Instruction[F] {
  def run(spin: Spin): F[Unit] =
    runner(mask, spin.or)(spin.consts)
  def runString(spin: Spin): F[Unit] =
    runner(mask, i => println(s"or($i)"))(spin.consts)
  override def toString: String = s"or($mask)"
  override def spinInstruction(): String = s"or ${mask.spinString}"
}

case class Loop[F[_]: Sync]() extends Instruction[F] {
  def run(spin: Spin) = Sync[F].unit
  def runString(spin: Spin) =
    Sync[F].delay(println("loop does nothing"))

  override def spinInstruction(): String = s"loop"
}

case class Jam[F[_]: Sync](lfo: Lfo) extends Instruction[F] {
  def run(spin: Spin) = runner(lfo, spin.jam)(spin.consts)
  def runString(spin: Spin) = runner(lfo, i => println(s"jam($i)"))(spin.consts)

  override def spinInstruction(): String = s"jam ${lfo.spinString}"
}

case class Not[F[_]: Sync]() extends Instruction[F] {
  def run(spin: Spin): F[Unit] = Sync[F].delay(spin.not())
  override def runString(spin: Spin): F[Unit] = Sync[F].delay(println("not()"))

  override def spinInstruction(): String = "not"
}
