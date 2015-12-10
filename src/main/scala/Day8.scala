object Day8 {

  def difference(filename: String, method: (String) => Int ): Int = {
    Utils.getStream(filename).getLines.foldLeft(0)((sum, line) => {
      val newLength = method(line)

      println( "" + line + " -> " + line.length() + " - " + newLength )

      sum + Math.abs( line.length() - newLength )
    })
  }

  def decodedLength(line: String): Int = {
    val rawLength: Int = line.length()
    val text = line.substring(1, rawLength - 1)
    val textLength = text.length()
    var i = 0
    var parsedLength = 0

    while( i < textLength ) {
      val character = text(i)

      character match {
        // Check next character.if a \ occurs
        case '\\' => text(i+1) match {
          case '\\' => {
            // Skip next character and increase length with 1
            parsedLength = parsedLength + 1
            i = i + 2
          }
          case 'x' => {
            // Skip next 4 characters and increase length with 1
            parsedLength = parsedLength + 1
            i = i + 4
          }
          case '"' => {
            // Skip next character and increase length with 1
            parsedLength = parsedLength + 1
            i = i + 2
          }
        }
        case _ => {
          parsedLength = parsedLength + 1
          i = i + 1
        }
      }
    }

    parsedLength
  }

  def encodedLength(line: String): Int = {
    val lengths = line.map {
      _ match {
        case '"' => 2
        case '\\' => 2
        case _ => 1
      }
    }

    return 2 + lengths.sum
  }

  def main(args: Array[String]) {
    val filename = if (args.size > 0) args(0) else "/day8/input.txt"
    val method: (String) => Int = if (args.size > 1 && args(1) == "decode" ) decodedLength else encodedLength

    val num = difference(filename, method)

    println("Difference: " + num)
  }
}