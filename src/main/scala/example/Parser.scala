//package example
//
//import org.andrewkilpatrick.elmGen.ElmProgram
//import org.andrewkilpatrick.elmGen.ElmProgram._
//import cats.syntax.either._
//
//import scala.util.Try
//
//class Parser(values: Map[String, String], doubles: Map[String, Double], memory: Map[String, Int]) extends ElmProgram("Parser") {
//
//  setSamplerate(41100)
//
//  case class Line(instruction: String, param: List[String])
//
//  def readLine(s: String) = {
//
//
//    val (instruction, params) = s.splitAt(s.indexOf("\t"))
//
//    val line = Line(instruction, params.split(",").toList.map(_.trim))
////
////    memory.toList.foreach {
////      case (name, value) => allocDelayMem(name, value)
////    }
//
//
//    line.instruction.trim match {
//      case "rdax" => handleRdax(twoParams(line.param))
//      case "wrax" => handleWrax(twoParams(line.param))
//      case "mulx" => handleMulx(oneParam(line.param))
//      case "rdfx" => handleRdfx(twoParams(line.param))
//      case "log"  => handleLog(twoParams(line.param))
//      case "exp"  => handleExp(twoParams(line.param))
//      case "sof"  => handleSof(twoParams(line.param))
//      case "wrap" => handleWrap(twoParams(line.param))
//      case "wra"  => handleWra(twoParams(line.param))
//      case "rda"  => handleRda(twoParams(line.param))
//      case "skp"  => handleSkp(twoParams(line.param))
//      case "wldr" => handleWldr(threeParams(line.param))
//      case "clr"  => clear()
//      case "cho"  => handleCho(line.param)
//      case _ => println(s"not implemented yet: $line")
//    }
//  }
//
//  def threeParams(params: List[String]): (String, String, String) = {
//    if (params.length != 3) {
//      println("cannot parse correct")
//      ("", "", "")
//    } else {
//      (params.head, params(1), params(2))
//    }
//  }
//
//  def twoParams(params: List[String]): (String, String) = {
//    if (params.length != 2) {
//      println("cannot parse correct")
//      ("", "")
//    } else {
//      (params.head, params.last)
//    }
//  }
//
//  def oneParam(params: List[String]): String = {
//    if (params.length != 1) {
//      println("cannot parse correct")
//      ""
//    } else {
//      params.head
//    }
//  }
//
//  def handleRdax(params: (String, String)) = {
//    (parseInt(params._1), asDouble(params._2)) match {
//      case (Right(i), Right(d)) =>
//        println(s"readRegister(${params._1}, ${params._2})")
//        readRegister(i, d)
//      case (Left(err), _) => println(s"cannot parse correct: $err")
//      case (_, Left(err)) => println(s"cannot parse correct: $err")
//    }
//  }
//
//  def handleRda(params: (String, String)) = {
//    (parseInt(params._1), asDouble(params._2)) match {
//      case (Right(i), Right(d)) =>
//        println(s"readDelay(${params._1}, ${params._2})")
//        readDelay(i, d)
//      case (Left(err), _)       => println(s"cannot parse correct: $err")
//      case (_, Left(err))       => println(s"cannot parse correct: $err")
//    }
//  }
//
//  def handleWrax(params: (String, String)) = {
//    (parseInt(params._1), asDouble(params._2)) match {
//      case (Right(i), Right(d)) =>
//        println(s"writeRegister(${params._1}, ${params._2})")
//        writeRegister(i, d)
//      case (Left(err), _) => println(s"cannot parse correct: $err")
//      case (_, Left(err)) => println(s"cannot parse correct: $err")
//    }
//  }
//
//  def handleMulx(param: String) = {
//    parseInt(param) match {
//      case Left(err) => println(s"cannot parse correct: $err")
//      case Right(i)  =>
//        println(s"mulx($i)")
//        mulx(i)
//    }
//  }
//
//  def handleRdfx(params: (String, String)) = {
//    (parseInt(params._1), asDouble(params._2)) match {
//      case (Right(i), Right(d)) =>
//        println(s"readRegisterFilter(${params._1}, ${params._2})")
//        readRegisterFilter(i, d)
//      case (Left(err), _) => println(s"cannot parse correct: $err")
//      case (_, Left(err)) => println(s"cannot parse correct: $err")
//    }
//  }
//
//  def handleLog(params: (String, String)) = {
//    (asDouble(params._1), asDouble(params._2)) match {
//      case (Right(d), Right(e)) =>
//        println(s"log(${params._1}, ${params._2})")
//        log(d, e)
//      case (Left(err), _) => println(s"cannot parse correct: $err")
//      case (_, Left(err)) => println(s"cannot parse correct: $err")
//    }
//  }
//
//  def handleExp(params: (String, String)) = {
//    (asDouble(params._1), asDouble(params._2)) match {
//      case (Right(d), Right(e)) =>
//        println(s"exp(${params._1}, ${params._2})")
//        exp(d, e)
//      case (Left(err), _) => println(s"cannot parse correct: $err")
//      case (_, Left(err)) => println(s"cannot parse correct: $err")
//    }
//  }
//
//  def handleSof(params: (String, String)) = {
//    (asDouble(params._1), asDouble(params._2)) match {
//      case (Right(d), Right(e)) =>
//        println(s"scaleOffset(${params._1}, ${params._2})")
//        scaleOffset(d, e)
//      case (Left(err), _) => println(s"cannot parse correct: $err")
//      case (_, Left(err)) => println(s"cannot parse correct: $err")
//    }
//  }
//
//  def handleWrap(params: (String, String)) = {
//    (parseInt(params._1), asDouble(params._2)) match {
//      case (Right(i), Right(d)) =>
//        println(s"writeAllPass(${params._1}, ${params._2})")
//        writeAllpass(i, d)
//      case (Left(err), _)       => println(s"cannot parse correct: $err")
//      case (_, Left(err))       => println(s"cannot parse correct: $err")
//    }
//  }
//
//  def handleWra(params: (String, String)) = {
//    (parseInt(params._1), asDouble(params._2)) match {
//      case (Right(i), Right(d)) =>
//        println(s"writeDelay(${params._1}, ${params._2})")
//        writeDelay(i, d)
//      case (Left(err), _)       => println(s"cannot parse correct: $err")
//      case (_, Left(err))       => println(s"cannot parse correct: $err")
//    }
//  }
//
//
//  def handleSkp(params: (String, String)) = {
//    (parseInt(params._1), parseInt(params._2)) match {
//      case (Right(i), Right(d)) =>
//        println(s"skip(${params._1}, ${params._2})")
//        skip(i, d)
//      case (Left(err), _)       => println(s"cannot parse correct: $err")
//      case (_, Left(err))       => println(s"cannot parse correct: $err")
//    }
//  }
//
//  def handleWldr(params: (String, String, String)) = {
//    (parseInt(params._1), parseInt(params._2), parseInt(params._3)) match {
//      case (Right(i), Right(j), Right(k)) =>
//        println(s"loadRampLFO($i, $j, $k)")
//        loadRampLFO(i, j, k)
//      case (Left(err), _, _)       => println(s"cannot parse correct: $err")
//      case (_, Left(err), _)       => println(s"cannot parse correct: $err")
//      case (_, _, Left(err))       => println(s"cannot parse correct: $err")
//    }
//  }
//
//  def handleCho(params: List[String]) = {
//    params match {
//      case "rda" :: tail =>
//        val p = threeParams(tail)
//
//        (parseInt(p._1), parseInt(p._2), parseInt(p._3)) match {
//          case (Right(i), Right(j), Right(k)) =>
//            println(s"chorusReadDelay($i, $j, $k)")
//            chorusReadDelay(i, j, k)
//          case (Left(err), _, _)       => println(s"cannot parse correct: $err")
//          case (_, Left(err), _)       => println(s"cannot parse correct: $err")
//          case (_, _, Left(err))       => println(s"cannot parse correct: $err")
//        }
//      case "sof" :: tail =>
//        val p = threeParams(tail)
//
//        (parseInt(p._1), parseInt(p._2), asDouble(p._3)) match {
//          case (Right(i), Right(j), Right(d)) =>
//            println(s"chorusScaleOffset($i, $j, $d)")
//            chorusScaleOffset(i, j, d)
//          case (Left(err), _, _)       => println(s"cannot parse correct: $err")
//          case (_, Left(err), _)       => println(s"cannot parse correct: $err")
//          case (_, _, Left(err))       => println(s"cannot parse correct: $err")
//        }
//      case head :: _ => println(s"not handling cho $head")
//    }
//  }
//
//  def asDouble(s: String) = {
//    val tryDouble = Try(s.toDouble).toEither
//    val tryDoubles = doubles.get(s).toRight(new Exception(s"Cannot parse double: $s"))
//
//    tryDouble.orElse(tryDoubles)
//  }
//
//  def parseInt(s: String): Either[Throwable, Int] = {
//
//    val tryInt = Try(s.toInt).toEither
//
//    val tryConst = s match {
//      case "adcl" => Right(ADCL)
//      case "adcr" => Right(ADCR)
//      case "dacl" => Right(DACL)
//      case "dacr" => Right(DACR)
//      case "pot0" => Right(POT0)
//      case "pot1" => Right(POT1)
//      case "pot2" => Right(POT2)
//      case "gez" => Right(SKP_GEZ)
//      case "run" => Right(SKP_RUN)
//      case "rmp0_rate" => Right(RMP0_RATE)
//      case "rmp0" => Right(CHO_LFO_RMP0)
//      case "compc" => Right(CHO_COMPC)
//      case "rptr2" => Right(CHO_RPTR2)
//      case "na" => Right(CHO_NA)
//      case _ => Left(new Exception(""))
//    }
//
//    val tryValues = values.get(s).toRight(new Exception("")).flatMap {
//      case "reg0" => Right(REG0)
//      case "reg1" => Right(REG1)
//      case "reg2" => Right(REG2)
//      case "reg3" => Right(REG3)
//      case "reg4" => Right(REG4)
//      case "reg5" => Right(REG5)
//      case "reg6" => Right(REG6)
//      case "reg7" => Right(REG7)
//      case "reg8" => Right(REG8)
//      case "reg9" => Right(REG9)
//      case "reg10" => Right(REG10)
//      case "reg11" => Right(REG11)
//      case "reg12" => Right(REG12)
//      case "reg13" => Right(REG13)
//      case "reg14" => Right(REG14)
//      case "reg15" => Right(REG15)
//      case "reg16" => Right(REG16)
//      case _ => Left(new Exception(""))
//    }
//
//    val tryMemory = memory.get(s).toRight(new Exception(s"Cannot parse int: $s"))
//
//    tryInt
//      .orElse(tryConst.orElse(tryValues.orElse(tryMemory)))
//
//  }
//
//}
