object Day10 {

  def lookAndSay(input: String): String = {
    val serieRegex = "^(\\d)(\\1*)(.*)".r

    input match {
      case serieRegex(digit,otherDigits,rest) => "" + (1+otherDigits.length) + digit + lookAndSay(rest)
      case _ => ""
    }
  }

  def main(args: Array[String]) {
    val input = if (args.size > 0) args(0) else "1131522"
    val iterations = if (args.size > 1) args(1).toInt else 1

    var current = input
    for( i <- 0 until iterations ) {
      current = lookAndSay(current)
    }

    println( "Input:   " + input )
    println( "Output:  " + current )
  }
}

// class Action(val inputs: List[Wire], val output: Wire, val action: (List[Wire]) => Wire ) {}