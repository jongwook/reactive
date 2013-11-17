package simulations

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CircuitSuite extends CircuitSimulator with FunSuite {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5
  
  test("andGate example") {
    val in1, in2, out = new Wire
    andGate(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run
    
    assert(out.getSignal === false, "and 1")

    in1.setSignal(true)
    run
    
    assert(out.getSignal === false, "and 2")

    in2.setSignal(true)
    run
    
    assert(out.getSignal === true, "and 3")
  }

  def toWire(values: Int*): List[Wire] = values.toList match {
    case Nil => Nil
    case head :: tail => {
      val wire = new Wire
      wire.setSignal(if (head != 0) true else false)
      wire :: toWire(tail:_*)
    }
  }

  test("orGate test") {
    val (in1, in2) = (new Wire(0), new Wire(0))
    val out = new Wire

    orGate(in1, in2, out)
    run

    assert(out.getSignal === false, "or 1")

    in1.setSignal(true)
    run

    assert(out.getSignal === true, "or 2")

    in2.setSignal(true)
    run

    assert(out.getSignal === true, "or 3")

    in1.setSignal(false)
    run

    assert(out.getSignal === true, "or 4")

    in2.setSignal(false)
    run

    assert(out.getSignal === false, "or 5");
  }


  test("demux") {
    val in = new Wire(1)
    val out = List(new Wire(1), new Wire(1), new Wire(1), new Wire(1))
    val c = List(new Wire(0), new Wire(0))

    demux(in, c, out)
    run

    assert(out(0).getSignal === false, "demux 1-0")
    assert(out(1).getSignal === false, "demux 1-1")
    assert(out(2).getSignal === false, "demux 1-2")
    assert(out(3).getSignal === true, "demux 1-3")

    c(0).setSignal(true)
    run

    assert(out(0).getSignal === false, "demux 2-0")
    assert(out(1).getSignal === true, "demux 2-1")
    assert(out(2).getSignal === false, "demux 2-2")
    assert(out(3).getSignal === false, "demux 2-3")


  }
}
