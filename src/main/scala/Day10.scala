object Day10 {

  def lookAndSay(input: String): String = {
    val serieRegex = "^(\\d)(\\1*)(.*)".r
    var output = new StringBuilder

    // Loop through the characters
    var currentChar = ' '
    var currentLength = 0

    for( i <- 0 until input.size ) {
      if( input(i) == currentChar ) {
        currentLength = currentLength + 1
      } else {
        // Store previous serie
        if( currentChar != ' ' ) {
          output.append(currentLength).append(currentChar)
        }

        currentChar = input(i)
        currentLength = 1
      }
    }

    // Append the last serie
    output.append(currentLength).append(currentChar)

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