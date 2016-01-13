object Day23 {
  val debug = true
  type RegisterMap = scala.collection.mutable.Map[String,Long]

  class ExecutionContext( var position: Int, val registers: RegisterMap ) {
    def executeInstruction(line: String): Boolean = {
      val halfRe    = "hlf (\\w+)".r
      val tripleRe  = "tpl (\\w+)".r
      val incRe     = "inc (\\w+)".r
      val jmpRe     = "jmp ([+-])(\\d+)".r
      val jieRe     = "jie (\\w+), ([+-])(\\d+)".r
      val jioRe     = "jio (\\w+), ([+-])(\\d+)".r

      if(debug) println( "l" + position.formatted( "%2d" ) + ": " + line )

      line match {
        case halfRe(register) => {
          if(debug) print( "  Half register " + register + " with value " + registers(register) )
          registers(register) = registers(register) / 2
          if(debug) println( " to: " + registers(register) )

          continue()
        }
        case tripleRe(register) => {
          if(debug) print( "  Triple register " + register + " with value " + registers(register) )
          registers(register) = registers(register) * 3
          if(debug) println( " to: " + registers(register) )
          continue()
        }
        case incRe(register) => {
          if(debug) print( "  Increment register " + register + " with value " + registers(register) )
          registers(register) = registers(register) + 1
          if(debug) println( " to: " + registers(register) )
          continue()
        }
        case jmpRe(sign, number) => jump(sign, number.toInt)
        case jioRe(register, sign, number) => registers(register) match {
          case 1 => jump(sign, number.toInt)
          case _ => continue()
        }
        case jieRe(register, sign, number) => ( registers(register) % 2 ) match {
          case 0 => jump(sign, number.toInt)
          case 1 => continue()
        }
      }

      true
    }

    def jump(sign: String, number: Int): Unit = {
      if(debug) println( "  Jumping " + sign + number + " positions from " + position)
      position = position + ( if(sign == "+" ) number else -number )
    }

    def continue(): Unit = {
      if(debug) println( "  Continue to next line")
      position = position + 1
    }
  }

  def execute(filename: String, map: RegisterMap): RegisterMap = {
    val instructions = Utils.getStream(filename).getLines.toList
    val currentContext = new ExecutionContext(0, map)

    while( currentContext.position < instructions.size ) {
      currentContext.executeInstruction( instructions(currentContext.position) )
    }

    currentContext.registers
  }

  def main(args: Array[String]) {
    val filename = if (args.size > 0) args(0) else "/day23/input.txt"
    val initialMap: RegisterMap = scala.collection.mutable.HashMap("a" -> 1, "b" -> 0)
    val resultingMap = execute(filename, initialMap)

    println( "Resulting registers: " + resultingMap)
  }
}
