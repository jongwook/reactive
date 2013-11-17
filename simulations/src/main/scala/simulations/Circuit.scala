package simulations

import common._

class Wire(var sigVal: Boolean = false) {
//  private var sigVal = false
  private var actions: List[Simulator#Action] = List()

  def this(value: Int) = this(if (value != 0) true else false)

  def getSignal: Boolean = sigVal
  
  def setSignal(s: Boolean) {
    if (s != sigVal) {
      sigVal = s
      actions.foreach(action => action())
    }
  }

  def addAction(a: Simulator#Action) {
    actions = a :: actions
    a()
  }
}

abstract class CircuitSimulator extends Simulator {

  val InverterDelay: Int
  val AndGateDelay: Int
  val OrGateDelay: Int

  def probe(name: String, wire: Wire) {
    wire addAction {
      () => afterDelay(0) {
        println(
          "  " + currentTime + ": " + name + " -> " +  wire.getSignal)
      }
    }
  }

  def buffer(input: Wire, output: Wire) {
    def bufferAction() {
      afterDelay(0) { output.setSignal(input.getSignal) }
    }
    input addAction bufferAction
  }

  def inverter(input: Wire, output: Wire) {
    def invertAction() {
      val inputSig = input.getSignal
      afterDelay(InverterDelay) { output.setSignal(!inputSig) }
    }
    input addAction invertAction
  }

  def andGate(a1: Wire, a2: Wire, output: Wire) {
    def andAction() {
      val a1Sig = a1.getSignal
      val a2Sig = a2.getSignal
      afterDelay(AndGateDelay) { output.setSignal(a1Sig & a2Sig) }
    }
    a1 addAction andAction
    a2 addAction andAction
  }

  //
  // to complete with orGates and demux...
  //

  def orGate(a1: Wire, a2: Wire, output: Wire) {
    def orAction() {
      val a1sig = a1.getSignal
      val a2sig = a2.getSignal
      afterDelay(OrGateDelay) { output.setSignal(a1sig | a2sig) }
    }
    a1 addAction orAction
    a2 addAction orAction
  }
  
  def orGate2(a1: Wire, a2: Wire, output: Wire) {
    val notA1 = new Wire
    val notA2 = new Wire
    val notA1AndNotA2 = new Wire
    inverter(a1, notA1)
    inverter(a2, notA2)
    andGate(notA1, notA2, notA1AndNotA2)
    inverter(notA1AndNotA2, output)
  }

  def demux(in: Wire, c: List[Wire], out: List[Wire]) {
    if ( out.length < (1 << c.length) )
      throw new IllegalArgumentException("Invalid input size")

    c match {
      case Nil => {
        buffer(in, out.head)
      }
      case head :: tail => {
        val half = (1 << c.length) / 2
        val out1 = out
        val out0 = out.drop(half)

        val notC = new Wire
        val in1 = new Wire
        val in0 = new Wire

        inverter(head, notC)

        andGate(in, head, in1)
        andGate(in, notC, in0)

        demux(in1, tail, out1)
        demux(in0, tail, out0)
      }
    }
  }

}

object Circuit extends CircuitSimulator {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5

  def andGateExample {
    val in1, in2, out = new Wire
    andGate(in1, in2, out)
    probe("in1", in1)
    probe("in2", in2)
    probe("out", out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    in1.setSignal(true)
    run

    in2.setSignal(true)
    run
  }

  //
  // to complete with orGateExample and demuxExample...
  //
}

object CircuitMain extends App {
  // You can write tests either here, or better in the test class CircuitSuite.
  Circuit.andGateExample
}
