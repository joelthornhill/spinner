package example
import org.andrewkilpatrick.elmGen.simulator.SpinSimulator

object Hello extends Greeting with App {
//
  val values = Map(
    "krt" -> "reg0",
    "kin" -> "reg1",
    "kmix" -> "reg2",
    "lpal" -> "reg4",
    "lpbl" -> "reg5",
    "lpar" -> "reg6",
    "lpbr" -> "reg7",
    "stop" -> "reg8",
    "pbyp" -> "reg9",
    "pol" -> "reg10",
    "por" -> "reg11",
    "kfl" -> "reg12",
    "temp" -> "reg13",
    "rmixl" -> "reg14",
    "rmixr" -> "reg15",
    "lbyp" -> "reg16"
  )

  val doubles = Map(
    "kap" -> 0.6,
    "-kap" -> -0.6,
    "kql" -> -0.2,
    "-kql" -> 0.2
  )

  val memory = Map(
    "ap1" -> 502,
    "ap2" -> 821,
    "dap1a" -> 2204,
    "dap1b" -> 2701,
    "del1" -> 4456,
    "dap2a" -> 2532,
    "dap2b" -> 2201,
    "del2" -> 6325,
    "pdelr" -> 4096,
    "pdelr+1" -> (4096 + 1),
    "pdell" -> 4096,
    "pdell+1" -> (4096 + 1),
    "dtemp" -> 1
  )

  val parser = new Parser(values, doubles, memory)

  val x = """rdax	pot0,1.999	;get pot0, clip the upper half of pot0's range.
            wrax	kmix,0		;write the output mix value
            rdax	pot0,-1.0		;get pot0 again, 0 to -1
            sof	1,0.999		;now +1 to 0
            sof	1.999,0		;now +1 until midpint, then decreases to 0
            wrax	kin,0		;write the input attenuator value
            rdax	pot0,1		;get pot0 again
            wrax	krt,1		;save in krt, keep in ACC
            sof	1,-0.5		;subtract 1/2
            skp	gez,2		;skp if pot is in upper half of range
            sof	0,0.5		;load accumulator with +0.5
            wrax	krt,0		;overwrite if pot is in lower half of range
            clr			;clr ACC from previous skp op
            rdax 	pot1,0.5
            sof 	1,-0.5		;(pot cannot go to full 1.0) -0.5 to 0
            wrax 	rmp0_rate,0
            rdax	pot1,1.999
            sof	-2.0,0
            sof	-2.0,0
            wrax	stop,0
            rdax	pot1,1
            sof	1.0,-1.0
            exp	1.0,0
            wrax	pbyp,0
            rdax	pot2,1		;get pot2
            sof	0.35,-0.35	;ranges -0.3 to 0
            exp	1,0
            wrax	kfl,0		;write to LP filter control
            rdax	pot2,1		;read pot2 (LP) again
            mulx 	pot2
            mulx	pot2
            mulx	pot2
            mulx	pot2
            wrax	lbyp,0
            rdax	adcl,0.25
            rdax	adcr,0.25	;get inputs, leave headroom
            mulx	kin		;scale by input attenuator
            rda	ap1,kap	;4 all passes:
            wrap	ap1,-kap
            rda	ap2,kap
            wrap	ap2,-kap
            wrax	temp,0		;write ap output to temp reg
            rda	del2,1
            mulx	krt
            rdax	temp,1
            rda	dap1a,kap
            wrap	dap1a,-kap
            rda	dap1b,kap
            wrap	dap1b,-kap
            wra	del1,0
            rda	del1,1
            mulx	krt
            rdax	temp,1
            rda	dap2a,kap
            wrap	dap2a,-kap
            rda	dap2b,kap
            wrap	dap2b,-kap
            wra	del2,0
            rdax	adcl,-1.0
            rda	del1,1.9
            mulx	pot0
            rdax	adcl,1
            wrax	rmixl,0
            rdax	adcr,-1.0
            rda	del2,1.9
            mulx	pot0
            rdax	adcr,1
            wrax	rmixr,0
            skp	run,1
            wldr	0,0,4096
            rdax	rmixl,1
            wra	pdell,0
            cho 	rda,rmp0,compc,pdell
            cho 	rda,rmp0,0,pdell+1
            wra 	dtemp,0
            cho 	rda,rmp0,compc,pdell
            cho 	rda,rmp0,rptr2,pdell+1
            cho 	sof,rmp0,compc,0
            cho 	rda,rmp0,na,dtemp
            mulx	stop
            wrax	temp,-1.0
            rdax	rmixl,1
            mulx	pbyp
            rdax	temp,1
            wrax 	pol,0
            rdax	rmixr,1
            wra	pdelr,0
            cho 	rda,rmp0,compc,pdelr
            cho 	rda,rmp0,0,pdelr+1
            wra 	dtemp,0
            cho 	rda,rmp0,compc,pdelr
            cho 	rda,rmp0,rptr2,pdelr+1
            cho 	sof,rmp0,compc,0
            cho 	rda,rmp0,na,dtemp
            mulx	stop
            wrax	temp,-1.0
            rdax	rmixr,1
            mulx	pbyp
            rdax	temp,1
            wrax	por,0
            rdax	lpal,1
            mulx	kfl
            rdax	lpbl,1
            wrax	lpbl,-1.0
            rdax	lpal,kql
            rdax	pol,1
            mulx	kfl
            rdax	lpal,1
            wrax	lpal,0
            rdax	lpar,1
            mulx	kfl
            rdax	lpbr,1
            wrax	lpbr,-1.0
            rdax	lpar,kql
            rdax	por,1
            mulx	kfl
            rdax	lpar,1
            wrax	lpar,0
            rdax	lpbl,-1.0
            rdax	pol,1
            mulx	lbyp
            rdax	lpbl,1
            wrax	dacl,0
            rdax	lpbr,-1.0
            rdax	por,1
            mulx	lbyp
            rdax	lpbr,1
            wrax	dacr,0""".stripMargin


   val lines = x.split("\n").toList.map(_.trim).map(removeComment)


  def removeComment(line: String) = {

    val index = line.indexOf(";")

    if (index == -1) line
    else line.splitAt(index)._1
  }
  lines.foreach(parser.readLine)


  // Simulate
//  val testWav = "/tmp/test.wav"
//  val outputFile = null
//  val sim = new SpinSimulator(Reverb, testWav, outputFile, 0.5, 0.6, 0.5)
//  sim.showInteractiveControls()
//  sim.showLevelLogger()
//  sim.setLoopMode(true)
//  sim.run()

}

trait Greeting {
  lazy val greeting: String = "hello"
}
