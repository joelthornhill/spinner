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
import Instruction._

sealed trait Instruction[F[_]] {
  def run(spin: Spin)(implicit c: Consts): F[Unit]
  def spinInstruction(): String = ""
}

object Instruction {
  type Consts = Map[String, InstructionValue]

  def printF[F[_]: Sync](s: String): F[Unit] = Sync[F].delay(println(s))

}

case class Rdax[F[_]: Sync](addr: Addr, scale: Scale) extends Instruction[F] {
  def run(spin: Spin)(implicit c: Consts): F[Unit] =
    runner(addr, scale, spin.readRegister _)

  override def toString = s"readRegister($addr, $scale)"
  override def spinInstruction(): String =
    s"rdax ${addr.spinString},${scale.spinString}"
}

case class Ldax[F[_]: Sync](addr: Addr) extends Instruction[F] {
  def run(spin: Spin)(implicit c: Consts): F[Unit] = addr.run(spin.loadAccumulator)
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

  override def toString: String = s"readDelay($addr, $scale)"
  override def spinInstruction(): String =
    s"rda ${addr.spinString}, ${scale.spinString}"
}

case class Wrax[F[_]: Sync](addr: Addr, scale: Scale) extends Instruction[F] {
  def run(spin: Spin)(implicit c: Consts): F[Unit] = runner(addr, scale, spin.writeRegister _)

  override def toString = s"writeRegister($addr, $scale)"
  override def spinInstruction(): String =
    s"wrax ${addr.spinString}, ${scale.spinString}"
}

case class Maxx[F[_]: Sync](addr: Addr, scale: Scale) extends Instruction[F] {
  def run(spin: Spin)(implicit c: Consts): F[Unit] =
    runner(addr, scale, spin.maxx _)
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

  override def toString: String = s"writeDelay($addr, $scale)"
  override def spinInstruction(): String =
    s"wra ${addr.spinString}, ${scale.spinString}"
}

case class Sof[F[_]: Sync](scale: Scale, offset: Offset) extends Instruction[F] {
  def run(spin: Spin)(implicit c: Consts): F[Unit] =
    runner(scale, offset, spin.scaleOffset _)
  override def toString: String = s"scaleOffset($scale, $offset)"
  override def spinInstruction(): String =
    s"sof ${scale.spinString},${offset.spinString}"
}

case class Equ[F[_]: Sync](name: String, value: EquValue) extends Instruction[F] {
  def run(spin: Spin)(implicit c: Consts): F[Unit] = Sync[F].unit // Do nothing
  override def toString = ""
  override def spinInstruction(): String = s"equ $name ${value.spinString}"
}

case class Mem[F[_]: Sync](name: String, value: EquValue) extends Instruction[F] {
  def run(spin: Spin)(implicit c: Consts): F[Unit] = value.run(spin.allocDelayMem(name, _))

  override def toString = s"allocDelayMem($name, $value)"
  override def spinInstruction(): String = s"mem $name  ${value.spinString}"
}

case class EOF[F[_]: Sync]() extends Instruction[F] {
  def run(spin: Spin)(implicit c: Consts): F[Unit] = Sync[F].unit
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

  override def toString: String = s"skp($flags, $nSkip)"
  override def spinInstruction(): String =
    s"skp ${flags.spinString},${nSkip.spinString}"
}

case class SkipLabel[F[_]: Sync](label: String) extends Instruction[F] {
  def run(spin: Spin)(implicit c: Consts): F[Unit] = Sync[F].unit

  override def spinInstruction(): String = s"$label:"
}

case class Clr[F[_]: Sync]() extends Instruction[F] {
  def run(spin: Spin)(implicit c: Consts): F[Unit] = Sync[F].delay(spin.clear())
  override def toString: String = "clear()"
  override def spinInstruction(): String = "clr"
}

case class Absa[F[_]: Sync]() extends Instruction[F] {
  def run(spin: Spin)(implicit c: Consts): F[Unit] = Sync[F].delay(spin.absa())
  override def toString: String = "absa()"
  override def spinInstruction(): String = "absa"
}

case class Exp[F[_]: Sync](scale: Scale, offset: Offset) extends Instruction[F] {
  def run(spin: Spin)(implicit c: Consts): F[Unit] =
    runner(scale, offset, spin.exp _)
  override def toString: String = s"exp($scale, $offset)"
  override def spinInstruction(): String =
    s"exp ${scale.spinString},${offset.spinString}"
}

case class Mulx[F[_]: Sync](addr: Addr) extends Instruction[F] {
  def run(spin: Spin)(implicit c: Consts): F[Unit] = addr.run(spin.mulx)
  override def toString: String = s"mulx($addr)"
  override def spinInstruction(): String = s"mulx ${addr.spinString}"
}

case class Xor[F[_]: Sync](mask: Mask) extends Instruction[F] {
  def run(spin: Spin)(implicit c: Consts): F[Unit] = mask.run(spin.xor)
  override def toString: String = s"xor($mask)"
  override def spinInstruction(): String = s"xor ${mask.spinString}"
}

case class Wldr[F[_]: Sync](lfo: Lfo, freq: Freq, amp: Amp) extends Instruction[F] {
  def run(spin: Spin)(implicit c: Consts): F[Unit] =
    runner(lfo, freq, amp, spin.loadRampLFO _)

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
  override def toString: String = s"chorusScaleOffset($lfo, $flags, $offset)"
  override def spinInstruction(): String =
    s"cho sof,${lfo.spinString},${flags.spinString},${offset.spinString}"
}

case class ChoRdal[F[_]: Sync](lfo: Lfo) extends Instruction[F] {
  def run(spin: Spin)(implicit c: Consts): F[Unit] = lfo.run(spin.chorusReadValue)
  override def toString: String = s"chorusReadValue($lfo)"
  override def spinInstruction(): String = s"cho rdal,${lfo.spinString}"
}

case class Wlds[F[_]: Sync](lfo: Lfo, freq: Freq, amp: Amp) extends Instruction[F] {
  def run(spin: Spin)(implicit c: Consts): F[Unit] =
    runner(lfo, freq, amp, spin.loadSinLFO(_: Int, _: Int, _: Int))
  override def toString: String = s"loadSinLFO($lfo, $freq, $amp)"
  override def spinInstruction(): String =
    s"wlds ${lfo.spinString},${freq.spinString},${amp.spinString}"
}

case class Rdfx[F[_]: Sync](addr: Addr, scale: Scale) extends Instruction[F] {
  def run(spin: Spin)(implicit c: Consts): F[Unit] =
    runner(addr, scale, spin.readRegisterFilter _)
  override def toString: String = s"readRegisterFilter($addr, $scale)"
  override def spinInstruction(): String =
    s"rdfx ${addr.spinString}, ${scale.spinString}"
}

case class Rmpa[F[_]: Sync](scale: Scale) extends Instruction[F] {
  def run(spin: Spin)(implicit c: Consts) = scale.run(spin.readDelayPointer)

  override def toString: String = s"readDelayPointer($scale)"
  override def spinInstruction(): String = s"rmpa ${scale.spinString}"
}

case class Wrlx[F[_]: Sync](addr: Addr, scale: Scale) extends Instruction[F] {
  def run(spin: Spin)(implicit c: Consts): F[Unit] =
    runner(addr, scale, spin.writeRegisterLowshelf _)
  override def toString: String = s"writeRegisterLowshelf($addr, $scale)"
  override def spinInstruction(): String =
    s"wrlx ${addr.spinString},${scale.spinString}"
}

case class Wrhx[F[_]: Sync](addr: Addr, scale: Scale) extends Instruction[F] {
  def run(spin: Spin)(implicit c: Consts): F[Unit] =
    runner(addr, scale, spin.writeRegisterHighshelf _)
  override def toString: String = s"writeRegisterHighshelf($addr, $scale)"
  override def spinInstruction(): String =
    s"wrhx ${addr.spinString}, ${scale.spinString}"
}

case class Log[F[_]: Sync](scale: Scale, offset: Offset) extends Instruction[F] {
  def run(spin: Spin)(implicit c: Consts): F[Unit] =
    runner(scale, offset, spin.log _)
  override def toString: String = s"log($scale, $offset)"
  override def spinInstruction(): String =
    s"log ${scale.spinString},${offset.spinString}"
}

case class And[F[_]: Sync](mask: Mask) extends Instruction[F] {
  def run(spin: Spin)(implicit c: Consts): F[Unit] = mask.run(spin.and)
  override def toString: String = s"and($mask)"
  override def spinInstruction(): String = s"and ${mask.spinString}"
}

case class OrInst[F[_]: Sync](mask: Mask) extends Instruction[F] {
  def run(spin: Spin)(implicit c: Consts): F[Unit] = mask.run(spin.or)
  override def toString: String = s"or($mask)"
  override def spinInstruction(): String = s"or ${mask.spinString}"
}

case class Loop[F[_]: Sync]() extends Instruction[F] {
  def run(spin: Spin)(implicit c: Consts) = Sync[F].unit
  override def spinInstruction(): String = s"loop"
}

case class Jam[F[_]: Sync](lfo: Lfo) extends Instruction[F] {
  def run(spin: Spin)(implicit c: Consts) = lfo.run(spin.jam)
  override def spinInstruction(): String = s"jam ${lfo.spinString}"
}

case class Not[F[_]: Sync]() extends Instruction[F] {
  def run(spin: Spin)(implicit c: Consts): F[Unit] = Sync[F].delay(spin.not())
  override def spinInstruction(): String = "not"
}
