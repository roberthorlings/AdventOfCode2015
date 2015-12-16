object Day16 {
  type Property = String
  class Aunt(val name: String, val properties: Map[Property, Int]) {
    override def toString() = name //  + ": " + properties.mkString(", ")

    // Returns a number about how well the pattern matches
    //    Each corresponding value will increase the score
    //    Each missing value will not do anything
    //    Each not-corresponding value will set the score to -1 (regardless of the rest)
    def patternScore(pattern: Pattern): Int = {
      val scores = pattern.properties map { case (property, value) =>
        properties.get(property) match {
          case Some(actualValue) if( value == actualValue ) => 1
          case Some(actualValue ) if( value != actualValue ) => -1
          case None => 0
        }
      }

      if(scores.exists( _ == -1 )) -1 else scores.sum
    }
  }

  // A pattern to search for is actually a prototype aunt
  type Pattern = Aunt

  def loadData(filename: String): Seq[Aunt] = {
    Utils.getStream(filename).getLines.map(parseLine(_)).filter( _.isDefined ).map( _.get ).toList
  }

  def parseLine(line: String): Option[Aunt] = {
    val lineRegex = "([\\w ]+): (.*)".r
    val propertyRegex = "(\\w+): (\\d+)".r

    line match {
      case lineRegex(name, properties) => Some(new Aunt(
        name,
        ( propertyRegex findAllIn properties ).map({
          case propertyRegex(propertyName, value) => propertyName -> value.toInt
        }).toMap
      ))
      case _ => println("Invalid syntax in input file: " + line); None
    }
  }

  def main(args: Array[String]) {
    val filename = if (args.size > 0) args(0) else "/day16/input.txt"
    val patternFilename = if (args.size > 1) args(1) else "/day16/pattern.txt"

    val aunts = loadData(filename)
    val patterns = loadData(patternFilename)

    for( pattern <- patterns ) {
      val best = aunts.maxBy(_.patternScore(pattern))
      println("Best aunt for pattern " + pattern + ": " + best + " with score " + best.patternScore(pattern))
    }

  }
}
