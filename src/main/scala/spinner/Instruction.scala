package spinner
import spinner.util.Helpers.getInt
import spinner.util.Helpers.handleAllOffsets
import spinner.util.Helpers.runner
import cats.effect.Sync
import cats.syntax.flatMap._
import cats.syntax.functor._
import spinner.Instruction.Consts
import spinner.Params._
import spinner.model.Addition
import spinner.model.DoubleValue
import spinner.model.InstructionValue
import spinner.model.StringValue
import spinner.model.WithArithmetic
import cats.syntax.flatMap._

sealed trait Instruction[F[_]] {
  def run(spin: Spin)(implicit c: Consts): F[Unit]
  def runString(spin: Spin)(implicit c: Consts): F[Unit]
  def spinInstruction(): String = ""
}

object Instruction {
  type Consts = Map[String, InstructionValue]
}

case class Rdax[F[_]: Sync](addr: Addr, scale: Scale) extends Instruction[F] {
  def run(spin: Spin)(implicit c: Consts): F[Unit] =
    runner(addr, scale, spin.readRegister _)
  def runString(spin: Spin)(implicit c: Consts): F[Unit] =
    runner(addr, scale, (i: Int, d: Double) => println(s"readRegister($i, $d)"))

  override def toString = s"readRegister($addr, $scale)"
  override def spinInstruction(): String =
    s"rdax ${addr.spinString},${scale.spinString}"
}

case class Ldax[F[_]: Sync](addr: Addr) extends Instruction[F] {
  def run(spin: Spin)(implicit c: Consts): F[Unit] = addr.run(spin.loadAccumulator)
  def runString(spin: Spin)(implicit c: Consts): F[Unit] =
    addr.run(a => println(s"loadAccumulator($a)"))

  override def toString: String = s"loadAccumulator($addr)"

  override def spinInstruction(): String = s"ldax ${addr.spinString}"
}

case class Rda[F[_]: Sync](addr: Addr, scale: Scale) extends Instruction[F] {
  def run(
    spin: Spin
  )(implicit c: Consts): F[Unit] =
    handleAllOffsets(
      addr,
      scale,
      spin.readDelay(_: String, _: Double, _: Double),
      spin.readDelay(_: String, _: Int, _: Double),
      spin.readDelay(_: Int, _: Double)
    )

  def runString(spin: Spin)(implicit c: Consts): F[Unit] =
    handleAllOffsets(
      addr,
      scale,
      (v, d1, d2) => println(s"readDelay($v, $d1, $d2)"),
      (v, i, d) => println(s"readDelay($v, $i, $d)"),
      (i, d) => println(s"readDelay($i, $d)")
    )

  override def toString: String = s"readDelay($addr, $scale)"
  override def spinInstruction(): String =
    s"rda ${addr.spinString}, ${scale.spinString}"
}

case class Wrax[F[_]: Sync](addr: Addr, scale: Scale) extends Instruction[F] {
  def run(spin: Spin)(implicit c: Consts): F[Unit] = runner(addr, scale, spin.writeRegister _)
  def runString(spin: Spin)(implicit c: Consts): F[Unit] =
    runner(addr, scale, (a: Int, b: Double) => println(s"writeRegister($a, $b)"))

  override def toString = s"writeRegister($addr, $scale)"
  override def spinInstruction(): String =
    s"wrax ${addr.spinString}, ${scale.spinString}"
}

case class Maxx[F[_]: Sync](addr: Addr, scale: Scale) extends Instruction[F] {
  def run(spin: Spin)(implicit c: Consts): F[Unit] =
    runner(addr, scale, spin.maxx _)
  def runString(spin: Spin)(implicit c: Consts): F[Unit] =
    runner(addr, scale, (i: Int, d: Double) => println(s"maxx($i, $d)"))
  override def toString = s"maxx($addr, $scale)"
  override def spinInstruction(): String =
    s"maxx ${addr.spinString}, ${scale.spinString}"
}

case class Wrap[F[_]: Sync](addr: Addr, scale: Scale) extends Instruction[F] {
  def run(
    spin: Spin
  )(implicit c: Consts): F[Unit] =
    handleAllOffsets(
      addr,
      scale,
      (v, d1, d2) => spin.writeAllpass(v, d1, d2),
      (v, i, d) => spin.writeAllpass(v, i, d),
      (i, d) => spin.writeAllpass(i, d)
    )
  def runString(spin: Spin)(implicit c: Consts): F[Unit] =
    handleAllOffsets(
      addr,
      scale,
      (v, d1, d2) => println(s"writeAllpass($v, $d1, $d2)"),
      (v, i, d) => println(s"writeAllpass($v, $i, $d)"),
      (i, d) => println(s"writeAllpass($i, $d)")
    )
  override def toString: String = s"writeAllpass($addr, $scale)"
  override def spinInstruction(): String =
    s"wrap ${addr.spinString}, ${scale.spinString}"
}

case class Wra[F[_]: Sync](addr: Addr, scale: Scale) extends Instruction[F] {
  def run(
    spin: Spin
  )(implicit c: Consts): F[Unit] =
    handleAllOffsets(
      addr,
      scale,
      (v, d1, d2) => spin.writeDelay(v, d1, d2),
      (v, i, d) => spin.writeDelay(v, i, d),
      (i, d) => spin.writeDelay(i, d)
    )

  def runString(spin: Spin)(implicit c: Consts): F[Unit] =
    handleAllOffsets(
      addr,
      scale,
      (v, d1, d2) => println(s"writeDelay($v, $d1, $d2)"),
      (v, i, d) => println(s"writeDelay($v, $i, $d)"),
      (i, d) => println(s"writeDelay($i, $d)")
    )

  override def toString: String = s"writeDelay($addr, $scale)"
  override def spinInstruction(): String =
    s"wra ${addr.spinString}, ${scale.spinString}"
}

case class Sof[F[_]: Sync](scale: Scale, offset: Offset) extends Instruction[F] {
  def run(spin: Spin)(implicit c: Consts): F[Unit] =
    runner(scale, offset, spin.scaleOffset _)
  def runString(spin: Spin)(implicit c: Consts): F[Unit] =
    runner(scale, offset, (d1: Double, d2: Double) => println(s"scaleOffset($d1, $d2)"))
  override def toString: String = s"scaleOffset($scale, $offset)"
  override def spinInstruction(): String =
    s"sof ${scale.spinString},${offset.spinString}"
}

case class Equ[F[_]: Sync](name: String, value: EquValue) extends Instruction[F] {
  def run(spin: Spin)(implicit c: Consts): F[Unit] = Sync[F].unit // Do nothing
  def runString(spin: Spin)(implicit c: Consts): F[Unit] =
    Sync[F].delay(println(s"val $name = ${value.spinString.toUpperCase}")) // Do nothing
  override def toString = ""
  override def spinInstruction(): String = s"equ $name ${value.spinString}"
}

case class Mem[F[_]: Sync](name: String, value: EquValue) extends Instruction[F] {
  def run(spin: Spin)(implicit c: Consts): F[Unit] = value.run(spin.allocDelayMem(name, _))
  def runString(spin: Spin)(implicit c: Consts) =
    value.run(i => println(s"allocDelayMem($name, $i)"))

  override def toString = s"allocDelayMem($name, $value)"
  override def spinInstruction(): String = s"mem $name  ${value.spinString}"
}

case class EOF[F[_]: Sync]() extends Instruction[F] {
  def run(spin: Spin)(implicit c: Consts): F[Unit] = Sync[F].unit
  def runString(spin: Spin)(implicit c: Consts) = Sync[F].unit
  override def spinInstruction(): String = ""
}

case class Skp[F[_]: Sync](flags: Flags, nSkip: NSkip) extends Instruction[F] {
  def run(
    spin: Spin
  )(implicit c: Consts): F[Unit] =
    for {
      flags <- getInt(flags.value)
      run <- nSkip.value match {
        case StringValue(_) => Sync[F].unit
        case _ =>
          getInt(nSkip.value).flatMap(nSkip => Sync[F].delay(spin.skip(flags, nSkip)))
      }
    } yield run

  def runString(spin: Spin)(implicit c: Consts) =
    for {
      flags <- getInt(flags.value)
      nSkip <- getInt(nSkip.value)
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
  def run(spin: Spin)(implicit c: Consts): F[Unit] = Sync[F].unit
  def runString(spin: Spin)(implicit c: Consts): F[Unit] =
    Sync[F].delay(println("SpinProgram"))

  override def spinInstruction(): String = s"$label:"
}

case class Clr[F[_]: Sync]() extends Instruction[F] {
  def run(spin: Spin)(implicit c: Consts): F[Unit] = Sync[F].delay(spin.clear())
  def runString(spin: Spin)(implicit c: Consts): F[Unit] =
    Sync[F].delay(println("clear()"))
  override def toString: String = "clear()"
  override def spinInstruction(): String = "clr"
}

case class Absa[F[_]: Sync]() extends Instruction[F] {
  def run(spin: Spin)(implicit c: Consts): F[Unit] = Sync[F].delay(spin.absa())
  def runString(spin: Spin)(implicit c: Consts): F[Unit] =
    Sync[F].delay(println("absa()"))
  override def toString: String = "absa()"
  override def spinInstruction(): String = "absa"
}

case class Exp[F[_]: Sync](scale: Scale, offset: Offset) extends Instruction[F] {
  def run(spin: Spin)(implicit c: Consts): F[Unit] =
    runner(scale, offset, spin.exp _)
  def runString(spin: Spin)(implicit c: Consts): F[Unit] =
    runner(scale, offset, (d1: Double, d2: Double) => println(s"exp($d1, $d2)"))
  override def toString: String = s"exp($scale, $offset)"
  override def spinInstruction(): String =
    s"exp ${scale.spinString},${offset.spinString}"
}

case class Mulx[F[_]: Sync](addr: Addr) extends Instruction[F] {
  def run(spin: Spin)(implicit c: Consts): F[Unit] = addr.run(spin.mulx)
  def runString(spin: Spin)(implicit c: Consts): F[Unit] = addr.run(i => println(s"mulx($i)"))
  override def toString: String = s"mulx($addr)"
  override def spinInstruction(): String = s"mulx ${addr.spinString}"
}

case class Xor[F[_]: Sync](mask: Mask) extends Instruction[F] {
  def run(spin: Spin)(implicit c: Consts): F[Unit] = mask.run(spin.xor)
  def runString(spin: Spin)(implicit c: Consts): F[Unit] = mask.run(i => println(s"xor($i)"))
  override def toString: String = s"xor($mask)"
  override def spinInstruction(): String = s"xor ${mask.spinString}"
}

case class Wldr[F[_]: Sync](lfo: Lfo, freq: Freq, amp: Amp) extends Instruction[F] {
  def run(spin: Spin)(implicit c: Consts): F[Unit] =
    runner(lfo, freq, amp, spin.loadRampLFO _)
  def runString(spin: Spin)(implicit c: Consts): F[Unit] =
    runner(lfo, freq, amp, (i1: Int, i2: Int, i3: Int) => println(s"loadRampLFO($i1, $i2, $i3)"))

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
  )(implicit c: Consts) =
    for {
      lfo <- getInt(lfo.value)
      flags <- getInt(flags.value)
      run <- addr.value match {
        case WithArithmetic(Addition(StringValue(value), DoubleValue(offset))) =>
          Sync[F].delay(spin.chorusReadDelay(lfo, flags, value, offset.toInt))
        case StringValue(value) =>
          Sync[F].delay(spin.chorusReadDelay(lfo, flags, value, 0))
        case _ =>
          getInt(addr.value).flatMap(addr => Sync[F].delay(spin.chorusReadDelay(lfo, flags, addr)))
      }
    } yield run

  def runString(spin: Spin)(implicit c: Consts) =
    for {
      lfo <- getInt(lfo.value)
      flags <- getInt(flags.value)
      run <- addr.value match {
        case WithArithmetic(Addition(StringValue(value), DoubleValue(offset))) =>
          Sync[F].delay(println(s"chorusReadDelay($lfo, $flags, $value, ${offset.toInt})"))
        case StringValue(value) =>
          Sync[F].delay(println(s"chorusReadDelay($lfo, $flags, $value, 0)"))
        case _ =>
          getInt(addr.value).flatMap(addr =>
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
  def run(spin: Spin)(implicit c: Consts): F[Unit] =
    runner(lfo, flags, offset, spin.chorusScaleOffset _)
  def runString(spin: Spin)(implicit c: Consts): F[Unit] =
    runner(
      lfo,
      flags,
      offset,
      (i1: Int, i2: Int, d1: Double) => println(s"chorusScaleOffset($i1, $i2, $d1)")
    )
  override def toString: String = s"chorusScaleOffset($lfo, $flags, $offset)"
  override def spinInstruction(): String =
    s"cho sof,${lfo.spinString},${flags.spinString},${offset.spinString}"
}

case class ChoRdal[F[_]: Sync](lfo: Lfo) extends Instruction[F] {
  def run(spin: Spin)(implicit c: Consts): F[Unit] = lfo.run(spin.chorusReadValue)
  def runString(spin: Spin)(implicit c: Consts): F[Unit] =
    lfo.run(i => println(s"chorusReadValue($i)"))
  override def toString: String = s"chorusReadValue($lfo)"
  override def spinInstruction(): String = s"cho rdal,${lfo.spinString}"
}

case class Wlds[F[_]: Sync](lfo: Lfo, freq: Freq, amp: Amp) extends Instruction[F] {
  def run(spin: Spin)(implicit c: Consts): F[Unit] =
    runner(lfo, freq, amp, spin.loadSinLFO(_: Int, _: Int, _: Int))
  def runString(spin: Spin)(implicit c: Consts): F[Unit] =
    runner(lfo, freq, amp, (i1: Int, i2: Int, i3: Int) => println(s"loadSinLFO($i1, $i2, $i3)"))
  override def toString: String = s"loadSinLFO($lfo, $freq, $amp)"
  override def spinInstruction(): String =
    s"wlds ${lfo.spinString},${freq.spinString},${amp.spinString}"
}

case class Rdfx[F[_]: Sync](addr: Addr, scale: Scale) extends Instruction[F] {
  def run(spin: Spin)(implicit c: Consts): F[Unit] =
    runner(addr, scale, spin.readRegisterFilter _)
  def runString(spin: Spin)(implicit c: Consts): F[Unit] =
    runner(addr, scale, (i: Int, d: Double) => println(s"readRegisterFilter($i, $d)"))
  override def toString: String = s"readRegisterFilter($addr, $scale)"
  override def spinInstruction(): String =
    s"rdfx ${addr.spinString}, ${scale.spinString}"
}

case class Rmpa[F[_]: Sync](scale: Scale) extends Instruction[F] {
  def run(spin: Spin)(implicit c: Consts) = scale.run(spin.readDelayPointer)
  def runString(spin: Spin)(implicit c: Consts): F[Unit] =
    scale.run(i => println(s"readDelayPointer($i)"))

  override def toString: String = s"readDelayPointer($scale)"
  override def spinInstruction(): String = s"rmpa ${scale.spinString}"
}

case class Wrlx[F[_]: Sync](addr: Addr, scale: Scale) extends Instruction[F] {
  def run(spin: Spin)(implicit c: Consts): F[Unit] =
    runner(addr, scale, spin.writeRegisterLowshelf _)
  def runString(spin: Spin)(implicit c: Consts): F[Unit] =
    runner(addr, scale, (i: Int, d: Double) => println(s"writeRegisterLowshelf($i, $d)"))
  override def toString: String = s"writeRegisterLowshelf($addr, $scale)"
  override def spinInstruction(): String =
    s"wrlx ${addr.spinString},${scale.spinString}"
}

case class Wrhx[F[_]: Sync](addr: Addr, scale: Scale) extends Instruction[F] {
  def run(spin: Spin)(implicit c: Consts): F[Unit] =
    runner(addr, scale, spin.writeRegisterHighshelf _)
  def runString(spin: Spin)(implicit c: Consts): F[Unit] =
    runner(addr, scale, (i: Int, d: Double) => println(s"writeRegisterHighshelf($i, $d)"))
  override def toString: String = s"writeRegisterHighshelf($addr, $scale)"
  override def spinInstruction(): String =
    s"wrhx ${addr.spinString}, ${scale.spinString}"
}

case class Log[F[_]: Sync](scale: Scale, offset: Offset) extends Instruction[F] {
  def run(spin: Spin)(implicit c: Consts): F[Unit] =
    runner(scale, offset, spin.log _)
  def runString(spin: Spin)(implicit c: Consts): F[Unit] =
    runner(scale, offset, (d1: Double, d2: Double) => println(s"log($d1, $d2)"))
  override def toString: String = s"log($scale, $offset)"
  override def spinInstruction(): String =
    s"log ${scale.spinString},${offset.spinString}"
}

case class And[F[_]: Sync](mask: Mask) extends Instruction[F] {
  def run(spin: Spin)(implicit c: Consts): F[Unit] = mask.run(spin.and)
  def runString(spin: Spin)(implicit c: Consts): F[Unit] = mask.run(i => println(s"and($i)"))
  override def toString: String = s"and($mask)"
  override def spinInstruction(): String = s"and ${mask.spinString}"
}

case class OrInst[F[_]: Sync](mask: Mask) extends Instruction[F] {
  def run(spin: Spin)(implicit c: Consts): F[Unit] = mask.run(spin.or)
  def runString(spin: Spin)(implicit c: Consts): F[Unit] = mask.run(i => println(s"or($i)"))
  override def toString: String = s"or($mask)"
  override def spinInstruction(): String = s"or ${mask.spinString}"
}

case class Loop[F[_]: Sync]() extends Instruction[F] {
  def run(spin: Spin)(implicit c: Consts) = Sync[F].unit
  def runString(spin: Spin)(implicit c: Consts) =
    Sync[F].delay(println("loop does nothing"))

  override def spinInstruction(): String = s"loop"
}

case class Jam[F[_]: Sync](lfo: Lfo) extends Instruction[F] {
  def run(spin: Spin)(implicit c: Consts) = lfo.run(spin.jam)
  def runString(spin: Spin)(implicit c: Consts) = lfo.run(i => println(s"jam($i)"))

  override def spinInstruction(): String = s"jam ${lfo.spinString}"
}

case class Not[F[_]: Sync]() extends Instruction[F] {
  def run(spin: Spin)(implicit c: Consts): F[Unit] = Sync[F].delay(spin.not())
  override def runString(spin: Spin)(implicit c: Consts): F[Unit] = Sync[F].delay(println("not()"))

  override def spinInstruction(): String = "not"
}
