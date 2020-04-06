package spinner
import java.util

import org.andrewkilpatrick.elmGen.MemSegment
import org.andrewkilpatrick.elmGen.SpinProgram
import spinner.model.InstructionValue

case class Spin(consts: Map[String, InstructionValue]) extends SpinProgram("Hello") {
  setSamplerate(44100)
}

object Spin {

  def copy(
    memoryMap: util.LinkedList[MemSegment],
    instList: util.LinkedList[org.andrewkilpatrick.elmGen.instructions.Instruction],
    consts: Map[String, InstructionValue]
  ): Spin = {
    val newInstructions = new Spin(consts)
    newInstructions.setMemoryMap(memoryMap)
    newInstructions.setInstList(instList)
    newInstructions
  }

}
