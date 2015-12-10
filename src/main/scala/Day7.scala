import logic._

object sim extends Gates with Parameters

object Day7 {

  def simulate(filename: String, initial: scala.collection.Map[String, Int] = scala.collection.immutable.Map.empty[String, Int]): scala.collection.Map[String, Int] = {
    val wires = scala.collection.mutable.HashMap.empty[String, sim.Wire]

    // Set initial value for wires
    for ((name, value) <- initial) {
      val wire = new sim.Wire
      wire setSignal value
      wires(name) = wire
    }

    Utils.getStream(filename).getLines.foreach(handleLine(_, wires))

    sim.run()

    wires.mapValues(_.getSignal)
  }

  def isAllDigits(x: String) = x forall Character.isDigit

  def handleLine[T](line: String, wires: scala.collection.mutable.Map[String, sim.Wire]): Unit = {
    // Define regular expressions for parsing each line
    val valRegex = "(\\d+) -> ([a-z]+)".r
    val assignmentRegex = "([a-z]+) -> ([a-z]+)".r
    val notRegex = "NOT ([a-z]+) -> ([a-z]+)".r
    val andRegex = "([a-z0-9]+) AND ([a-z0-9]+) -> ([a-z]+)".r
    val orRegex = "([a-z0-9]+) OR ([a-z0-9]+) -> ([a-z]+)".r
    val lsRegex = "([a-z]+) LSHIFT (\\d+) -> ([a-z]+)".r
    val rsRegex = "([a-z]+) RSHIFT (\\d+) -> ([a-z]+)".r

    // Shortcut method for retrieving the actual wire
    def wire(name: String) = wires.getOrElseUpdate(name, new sim.Wire)
    def constant(value: Int): sim.Wire = {
      val w = new sim.Wire
      w setSignal value
      return w
    }

    // Do the matching
    line match {
      case valRegex(value, output) => wire(output) setSignal value.toInt;
      case assignmentRegex(input, output) => wires(output) = wire(input)
      case notRegex(input, output) => sim.inverter(wire(input), wire(output))
      case andRegex(input1, input2, output) => {
        val in1 = if (isAllDigits(input1)) constant(input1.toInt) else wire(input1)
        val in2 = if (isAllDigits(input2)) constant(input2.toInt) else wire(input2)
        sim.andGate(in1, in2, wire(output))
      }
      case orRegex(input1, input2, output) => {
        val in1 = if (isAllDigits(input1)) constant(input1.toInt) else wire(input1)
        val in2 = if (isAllDigits(input2)) constant(input2.toInt) else wire(input2)
        sim.orGate(in1, in2, wire(output))
      }
      case lsRegex(input, bits, output) => sim.lShiftGate(wire(input), bits.toInt, wire(output))
      case rsRegex(input, bits, output) => sim.rShiftGate(wire(input), bits.toInt, wire(output))
      case _ => println("Line [" + line + "] is not matched by any of the rules. May be invalid!")

    }
  }

  def main(args: Array[String]) {
    val filename = if (args.size > 0) args(0) else "/day7/input.txt"
    val initFile = if (args.size > 1) args(1) else "/day7/init.txt"
    val numIterations = if (args.size > 2) args(2).toInt else 10

    // Determine the initial data
    var initial = simulate(initFile)

    // Now simulate until convergence
    var currentA = -1;
    var output: scala.collection.Map[String, Int] = Map.empty
    var iteration = 0
    do {
      iteration = iteration + 1
      output = simulate(filename, initial)

      // Send output of A to wire b to restart the simulation
      initial = Map("b" -> output("a"))

      println("  a = " + output("a"))
    } while (iteration < numIterations && output("a") != currentA)


    for ((name, value) <- output)
      println("[" + name + "] -> " + value)

    println("------------------")
    println("Wire a: " + output("a"))
  }
}

// class Action(val inputs: List[Wire], val output: Wire, val action: (List[Wire]) => Wire ) {}