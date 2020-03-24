package example
import enumeratum.EnumEntry
import enumeratum._
import enumeratum.EnumEntry._
import example.ParserCombinator.Reserved

sealed trait ReservedWord extends EnumEntry {
  val value: Int
}

object ReservedWord extends Enum[ReservedWord] {

  val values = findValues

  case object SIN0_RATE extends ReservedWord {
    val value = 0
  }
  case object SIN0_RANGE extends ReservedWord {
    val value = 1
  }
  case object SIN1_RATE extends ReservedWord {
    val value = 2
  }
  case object SIN1_RANGE extends ReservedWord {
    val value = 3
  }

  case object RMP0_RATE extends ReservedWord {
    val value = 4
  }
  case object RMP0_RANGE extends ReservedWord {
    val value = 5
  }
  case object RMP1_RATE extends ReservedWord {
    val value = 6
  }
  case object RMP1_RANGE extends ReservedWord {
    val value = 7
  }

  case object POT0 extends ReservedWord {
    val value = 16
  }
  case object POT1 extends ReservedWord {
    val value = 17
  }
  case object POT2 extends ReservedWord {
    val value = 18
  }

  case object ADCL extends ReservedWord {
    val value = 20
  }
  case object ADCR extends ReservedWord {
    val value = 21
  }

  case object DACL extends ReservedWord {
    val value = 22
  }
  case object DACR extends ReservedWord {
    val value = 23
  }

  case object ADDR_PTR extends ReservedWord {
    val value = 24
  }

  case object REG0 extends ReservedWord {
    val value = 32
  }
  case object REG1 extends ReservedWord {
    val value = 33
  }
  case object REG2 extends ReservedWord {
    val value = 34
  }
  case object REG3 extends ReservedWord {
    val value = 35
  }
  case object REG4 extends ReservedWord {
    val value = 36
  }
  case object REG5 extends ReservedWord {
    val value = 37
  }
  case object REG6 extends ReservedWord {
    val value = 38
  }
  case object REG7 extends ReservedWord {
    val value = 39
  }
  case object REG8 extends ReservedWord {
    val value = 40
  }
  case object REG9 extends ReservedWord {
    val value = 41
  }
  case object REG10 extends ReservedWord {
    val value = 42
  }
  case object REG11 extends ReservedWord {
    val value = 43
  }
  case object REG12 extends ReservedWord {
    val value = 44
  }
  case object REG13 extends ReservedWord {
    val value = 45
  }
  case object REG14 extends ReservedWord {
    val value = 46
  }
  case object REG15 extends ReservedWord {
    val value = 47
  }
  case object REG16 extends ReservedWord {
    val value = 48
  }
  case object REG17 extends ReservedWord {
    val value = 49
  }
  case object REG18 extends ReservedWord {
    val value = 50
  }

  case object REG19 extends ReservedWord {
    val value = 51
  }
  case object REG20 extends ReservedWord {
    val value = 52
  }
  case object REG21 extends ReservedWord {
    val value = 53
  }
  case object REG22 extends ReservedWord {
    val value = 54
  }
  case object REG23 extends ReservedWord {
    val value = 55
  }
  case object REG24 extends ReservedWord {
    val value = 56
  }
  case object REG25 extends ReservedWord {
    val value = 57
  }
  case object REG26 extends ReservedWord {
    val value = 58
  }
  case object REG27 extends ReservedWord {
    val value = 59
  }
  case object REG28 extends ReservedWord {
    val value = 60
  }
  case object REG29 extends ReservedWord {
    val value = 61
  }
  case object REG30 extends ReservedWord {
    val value = 62
  }
  case object REG31 extends ReservedWord {
    val value = 63
  }
  case object NEG extends ReservedWord {
    val value = 1
  }
  case object GEZ extends ReservedWord {
    val value = 2
  }
  case object ZRO extends ReservedWord {
    val value = 4
  }
  case object ZRC extends ReservedWord {
    val value = 8
  }
  case object RUN extends ReservedWord {
    val value = 16
  }
  case object SIN0 extends ReservedWord {
    val value = 0
  }
  case object SIN1 extends ReservedWord {
    val value = 1
  }
  case object RMP0 extends ReservedWord {
    val value = 2
  }
  case object RMP1 extends ReservedWord {
    val value = 3
  }
  case object COS0 extends ReservedWord {
    val value = 4
  }
  case object COS1 extends ReservedWord {
    val value = 5
  }
  case object SIN extends ReservedWord {
    val value = 0
  }
  case object COS extends ReservedWord {
    val value = 1
  }
  case object REG extends ReservedWord {
    val value = 2
  }
  case object COMPC extends ReservedWord {
    val value = 4
  }
  case object COMPA extends ReservedWord {
    val value = 8
  }
  case object RPTR2 extends ReservedWord {
    val value = 16
  }
  case object NA extends ReservedWord {
    val value = 32
  }
}