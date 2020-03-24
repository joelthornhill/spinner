package example
import example.EquParser.EquValue
import example.Instructions.{Equ, Instruction}

object App extends SpinParser with EquParser {

  def removeComment(line: String) = {

    val index = line.indexOf(";")

    if (index == -1) line
    else line.splitAt(index)._1
  }

  def main(args: Array[String]) = {

    val s: String =
      """;sample reverb program for FV-1
        ;minimize number of delays and ops.
        ;4 aps driving 2 AP-delay loops
        ;drive both loop elements, take output from each
        ;no pot controls
        ;output is full reverb, not mixed
        ;22 operations (of 128)

        mem	api1	122
        mem	api2	303
        mem	api3	553
        mem	api4	922

        mem	ap1	3823
        mem	del1	8500	;input = left output

        mem	ap2	4732
        mem	del2	7234	;input = right output
        ;input all passes (2)

        rdax	adcl,0.25	;read inputs,
        rdax	adcr,0.25	;attenuate, sum and
        rda	api1#,kap	;do 4 APs
        wrap	api1,-kap
        rda	api2#,kap
        wrap	api2,-cap
        rda	api3#,kap
        wrap	api3,-kap
        rda	api4#,kap
        wrap	api4,-kap
        wrax	apout,1		;write to min, keep in ACC

        ;first loop apd:
        			;AP'd input in ACC
        rda	del2#,krt	;read del2, scale by Krt
        rda	ap1#,-kap	;do loop ap
        wrap	ap1,kap
        wra	del1,1.99	;write delay, x2 for dac out
        wrax	dacl,0

        ;second loop apd:

        rdax	apout,1		;get input signal again
        rda	del1#,krt	;as above, to other side of loop
        rda	ap2#,kap
        wrap	ap2,-kap
        wra	del2,1.99
        wrax	dacr,0
      """.stripMargin

    val getLines: List[String] = s.split("\n").toList
      .map(removeComment)
      .filterNot(_.startsWith(";"))
      .filterNot(_.isEmpty)

    val equParse: Map[String, EquValue] = getLines.flatMap(line =>
      parse(equParser, line) match {
        case Success(matched: Map[String, EquValue], _) => Some(matched)
        case Failure(msg, _) => println(s"FAILURE: $msg"); None
        case Error(msg, _) => println(s"ERROR: $msg"); None
      }
    ).flatten.toMap


    val spinParse: List[Instruction] = getLines.flatMap(line =>
      parse(spinParser, line) match {
        case Success(matched, _) => Some(matched)
        case Failure(msg, _)     => println(s"FAILURE: $msg"); None
        case Error(msg, _)       => println(s"ERROR: $msg"); None
      }
    )


    spinParse.foreach { a =>
      println(a.toString)
      a.run(equParse )
    }

    println("Done")



  }
}