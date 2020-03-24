//package example
//import cats.effect.{ExitCode, IO, IOApp}
//import example.EquParser.EquValue
//import cats.implicits._
//import example.Instruction.Instruction
//
//object App extends IOApp with EquParser {
//
//  def removeComment(line: String) = {
//
//    val index = line.indexOf(";")
//
//    if (index == -1) line
//    else line.splitAt(index)._1
//  }
//
//  def printVals(m: Map[String, EquValue]): IO[List[Unit]] =
//    m.toList.map { value =>
//      IO(println(s"val ${value._1} = ${value._2}"))
//    }.sequence
//
//  def printInstructions(instructions: List[Instruction]): IO[List[Unit]] =
//    instructions.map(i => IO(println(i))).sequence
//
//  def runInstructions(instructions: List[Instruction], constants: Map[String, EquValue]): IO[List[Unit]] =
//    instructions.map(i => IO(i.run(constants))).sequence
//
//  def run(args: List[String]): IO[ExitCode] = {
//
//    val s: String =
//      """;disco mixer program
//        ;pot0 = infinite reverb
//        ;pot1 = pitch to zero
//        ;pot2 = 4 pole low pass filter
//
//        equ	krt	reg0
//        equ	kin	reg1
//        equ	kmix	reg2
//        equ	lpal	reg4
//        equ	lpbl	reg5
//        equ	lpar	reg6
//        equ	lpbr	reg7
//        equ	stop	reg8
//        equ	pbyp	reg9
//        equ	pol	reg10
//        equ	por	reg11
//        equ	kfl	reg12
//        equ	temp	reg13
//        equ	rmixl	reg14
//        equ	rmixr	reg15
//        equ	lbyp	reg16
//
//
//        mem	ap1	502
//        mem	ap2	821
//
//        mem	dap1a	2204
//        mem	dap1b	2701
//        mem	del1	4456
//        mem	dap2a	2532
//        mem	dap2b	2201
//        mem	del2	6325
//
//        mem	pdelr	4096
//        mem	pdell	4096
//        mem	dtemp	1
//
//        equ	kap	0.6
//        equ	kql	-0.2
//
//        ;prepare pots to affect control variables:
//        ;pot0 controls reverb time, but also affects input drive level;
//        ;reveb time is moderate up to about mid position, then increases
//        ;to infinity (or nearly) at full position.
//        ;input drive is constant, but decreases at the full pot0 position.
//        ;output mix is varied over the first half of pot0, then remains
//        ;high to the end of pot0's range.
//
//        rdax	pot0,1.999	;get pot0, clip the upper half of pot0's range.
//        wrax	kmix,0		;write the output mix value
//
//        rdax	pot0,-1.0		;get pot0 again, 0 to -1
//        sof	1,0.999		;now +1 to 0
//        sof	1.999,0		;now +1 until midpint, then decreases to 0
//        wrax	kin,0		;write the input attenuator value
//
//        rdax	pot0,1		;get pot0 again
//        wrax	krt,1		;save in krt, keep in ACC
//        sof	1,-0.5		;subtract 1/2
//        skp	gez,2		;skp if pot is in upper half of range
//        sof	0,0.5		;load accumulator with +0.5
//        wrax	krt,0		;overwrite if pot is in lower half of range
//
//        ;now prepare pot1 for pitch to zero.
//        ;counter clockwise is full stop, clockwise is normal run
//
//        clr			;clr ACC from previous skp op
//        rdax 	pot1,0.5
//        sof 	1,-0.5		;(pot cannot go to full 1.0) -0.5 to 0
//        wrax 	rmp0_rate,0
//
//        ;prepare stop signal, which shuts off the signal at the
//        ;stop end of the pot range:
//
//        rdax	pot1,1.999
//        sof	-2.0,0
//        sof	-2.0,0
//        wrax	stop,0
//
//        ;and a bypass value at norm pitch:
//
//        rdax	pot1,1
//        sof	1.0,-1.0
//        exp	1.0,0
//        wrax	pbyp,0
//
//        ;prepare pot2 for low pass frequency control:
//
//        rdax	pot2,1		;get pot2
//        sof	0.35,-0.35	;ranges -0.3 to 0
//        exp	1,0
//        wrax	kfl,0		;write to LP filter control
//
//        ;now derive filter bypass function (at open condition)
//
//        rdax	pot2,1		;read pot2 (LP) again
//        mulx 	pot2
//        mulx	pot2
//        mulx	pot2
//        mulx	pot2
//        wrax	lbyp,0
//
//        ;now do reverb, simple, twin loop, mono drive:
//
//        rdax	adcl,0.25
//        rdax	adcr,0.25	;get inputs, leave headroom
//        mulx	kin		;scale by input attenuator
//        rda	ap1#,kap	;4 all passes:
//        wrap	ap1,-kap
//        rda	ap2#,kap
//        wrap	ap2,-kap
//        wrax	temp,0		;write ap output to temp reg
//
//        rda	del2#,1
//        mulx	krt
//        rdax	temp,1
//        rda	dap1a#,kap
//        wrap	dap1a,-kap
//        rda	dap1b#,kap
//        wrap	dap1b,-kap
//        wra	del1,0
//        rda	del1#,1
//        mulx	krt
//        rdax	temp,1
//        rda	dap2a#,kap
//        wrap	dap2a,-kap
//        rda	dap2b#,kap
//        wrap	dap2b,-kap
//        wra	del2,0
//
//        ;now mix the inputs with the reverb:
//
//        rdax	adcl,-1.0
//        rda	del1,1.9
//        mulx	pot0
//        rdax	adcl,1
//        wrax	rmixl,0
//
//        rdax	adcr,-1.0
//        rda	del2,1.9
//        mulx	pot0
//        rdax	adcr,1
//        wrax	rmixr,0
//
//        ;Reverb outputs are at rmixl and rmixr.
//        ;now do pitch to zero:
//
//        skp	run,1
//        wldr	0,0,4096
//
//        rdax	rmixl,1
//        wra	pdell,0
//
//        cho 	rda,rmp0,reg|compc,pdell
//        cho 	rda,rmp0,0,pdell+1
//        wra 	dtemp,0
//        cho 	rda,rmp0,rptr2|compc,pdell
//        cho 	rda,rmp0,rptr2,pdell+1
//        cho 	sof,rmp0,na|compc,0
//        cho 	rda,rmp0,na,dtemp
//        mulx	stop
//        wrax	temp,-1.0
//        rdax	rmixl,1
//        mulx	pbyp
//        rdax	temp,1
//        wrax 	pol,0
//
//        rdax	rmixr,1
//        wra	pdelr,0
//
//        cho 	rda,rmp0,reg|compc,pdelr
//        cho 	rda,rmp0,0,pdelr+1
//        wra 	dtemp,0
//        cho 	rda,rmp0,rptr2|compc,pdelr
//        cho 	rda,rmp0,rptr2,pdelr+1
//        cho 	sof,rmp0,na|compc,0
//        cho 	rda,rmp0,na,dtemp
//        mulx	stop
//        wrax	temp,-1.0
//        rdax	rmixr,1
//        mulx	pbyp
//        rdax	temp,1
//        wrax	por,0
//
//        rdax	lpal,1
//        mulx	kfl
//        rdax	lpbl,1
//        wrax	lpbl,-1.0
//        rdax	lpal,kql
//        rdax	pol,1
//        mulx	kfl
//        rdax	lpal,1
//        wrax	lpal,0
//
//        rdax	lpar,1
//        mulx	kfl
//        rdax	lpbr,1
//        wrax	lpbr,-1.0
//        rdax	lpar,kql
//        rdax	por,1
//        mulx	kfl
//        rdax	lpar,1
//        wrax	lpar,0
//
//        rdax	lpbl,-1.0
//        rdax	pol,1
//        mulx	lbyp
//        rdax	lpbl,1
//        wrax	dacl,0
//
//        rdax	lpbr,-1.0
//        rdax	por,1
//        mulx	lbyp
//        rdax	lpbr,1
//        wrax	dacr,0
//
//      """.stripMargin
//
//    val getLines: List[String] = s.split("\n").toList
//      .map(removeComment)
//      .filterNot(_.startsWith(";"))
//      .filterNot(_.isEmpty)
//
//    val constants: IO[Map[String, EquValue]] = IO(
//      getLines
//        .flatMap(
//          line =>
//            parse(equParser, line) match {
//              case Success(matched: Map[String, EquValue], _) => Some(matched)
//              case Failure(msg, _)                            => println(s"FAILURE: $msg"); None
//              case Error(msg, _)                              => println(s"ERROR: $msg"); None
//          }
//        )
//        .flatten
//        .toMap
//    )
//
//    val instructions = new Instructions()
//    val spinParser = new SpinParser(instructions)
//
//    val spinParse: IO[List[Instruction]] = IO(getLines
//      .flatMap(
//        line =>
//          parse(spinParser.spinParser., line) match {
//            case Success(_: instructions.Equ, _) | Success(instructions.EOF, _) =>  None
//            case Success(matched, _) => Some(matched)
//            case Failure(msg, _)     => println(s"FAILURE: $msg"); None
//            case Error(msg, _)       => println(s"ERROR: $msg"); None
//        }
//      ))
//
//
//    val testWave = "/tmp/test.wav"
//
//
//
//    val program = for {
//      consts <- constants
//      _ <- printVals(consts)
//      instructions <- spinParse
//      _ <- printInstructions(instructions)
//      _ <- runInstructions(instructions, consts)
//      _ <- IO(println("Done"))
////      sim <- IO(new SpinSim)
//    } yield ()
//
////    import org.andrewkilpatrick.elmGen.simulator.SpinSimulator
////    val testWav = "c:\\temp\\test1.wav" // input test file name
////    //			String outputFile = "c:\\temp\\out.wav";  // write out to a file
////    val outputFile = null // play out through the sound card
////    val sim =
////      new SpinSimulator(reverb, testWav, outputFile, 0.5, 0.6, 0.25)
////    sim.showInteractiveControls() //
////
////    sim.showLevelLogger()
////    sim.setLoopMode(true)
//
//    program.as(ExitCode.Success)
//
//  }
//}