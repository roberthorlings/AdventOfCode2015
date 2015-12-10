package logic

abstract class Gates extends Simulation {
  class Wire {
    private var sigVal: Char = 0
    private var actions: List[Action] = List()

    def getSignal = sigVal

    def setSignal(s: Char) =
      if (s != sigVal) {
        sigVal = s
        actions foreach (_())
      }

    def setSignal(i: Int): Unit = setSignal( i.toChar )

    def addAction(a: Action) = {
      actions = a :: actions
      a()
    }
  }

  def InverterDelay: Int
  def AndGateDelay: Int
  def OrGateDelay: Int
  def ShiftGateDelay: Int
  def AssignmentDelay: Int

  def inverter(input: Wire, output: Wire): Unit = {
    def invertAction(): Unit = {
      val inputSig = input.getSignal
      afterDelay(InverterDelay) {
        output setSignal ~inputSig
      }
    }
    input addAction invertAction
  }

  def assign(input: Wire, output: Wire): Unit = {
    def assignAction(): Unit = {
      val inputSig = input.getSignal
      afterDelay(AssignmentDelay) {
        output setSignal inputSig
      }
    }
    input addAction assignAction
  }

  def andGate(in1: Wire, in2: Wire, output: Wire) = {
    def andAction() = {
      val in1Sig = in1.getSignal
      val in2Sig = in2.getSignal
      afterDelay(AndGateDelay) {
        output setSignal (in1Sig & in2Sig)
      }
    }
    in1 addAction andAction
    in2 addAction andAction
  }

  def orGate(in1: Wire, in2: Wire, output: Wire): Unit = {
    def orAction() = {
      val in1Sig = in1.getSignal
      val in2Sig = in2.getSignal
      afterDelay(OrGateDelay) {
        output setSignal (in1Sig | in2Sig)
      }
    }
    in1 addAction orAction
    in2 addAction orAction
  }

  def lShiftGate(in: Wire, bits: Int, output: Wire): Unit = {
    def lShiftAction() = {
      val inSig = in.getSignal
      afterDelay(ShiftGateDelay) {
        output setSignal inSig << bits
      }
    }
    in addAction lShiftAction
  }

  def rShiftGate(in: Wire, bits: Int, output: Wire): Unit = {
    def rShiftAction() = {
      val inSig = in.getSignal
      afterDelay(ShiftGateDelay) {
        output setSignal inSig >> bits
      }
    }
    in addAction rShiftAction
  }

  def probe(name: String, wire: Wire): Unit = {
    def probeAction(): Unit = {
      println(name + " " + currentTime + " new-value = " + wire.getSignal)
    }
    wire addAction probeAction
  }

}
