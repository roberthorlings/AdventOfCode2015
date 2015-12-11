object Day10 {

  def lookAndSay(input: String): String = {
    val serieRegex = "^(\\d)(\\1*)(.*)".r
    var output = new StringBuilder
    var current = input

    while (current.length > 0) {
      current match {
        case serieRegex(digit, otherDigits, rest) => {
          output.append(1 + otherDigits.length).append(digit)
          current = rest
        }
        case _ => return output.toString()
      }
    }

    return output.toString()
  }

  def main(args: Array[String]) {
    val input = if (args.size > 0) args(0) else "1131522"
    val iterations = if (args.size > 1) args(1).toInt else 1

    var current = input
    for( i <- 0 until iterations ) {
      println( "Iteration " + i )
      current = lookAndSay(current)
    }

    println( "Input:   " + input )
    //println( "Output:  " + current )
    println( "Length:  " + current.length )
  }
}

// class Action(val inputs: List[Wire], val output: Wire, val action: (List[Wire]) => Wire ) {}