object Day19 {
  class Rule(val original: String, val replacement: String) {}

  // A chromosome is actually just a string
  type Chromosome = String

  def loadData(filename: String): Tuple2[Seq[Rule], Chromosome] = {
    val rules = scala.collection.mutable.MutableList.empty[Rule]
    var chromosome = ""

    for( line <- Utils.getStream(filename).getLines ) {
      parseRule(line) match {
        case Some(rule) => rules += rule
        case None if( line != "" ) => chromosome = line
        case _ =>
      }
    }

    (rules.toList, chromosome)
  }

  def parseRule(line: String): Option[Rule] = {
    val ruleRegex= "([\\w ]+) => ([\\w ]+)".r

    line match {
      case ruleRegex(original, replacement) => Some(new Rule( original, replacement ))
      case _ => println("no rule found in input file for line: " + line); None
    }
  }

  def iterate(input: Chromosome, rules: Map[String, Traversable[Rule]]): Set[Chromosome] = {
    if( input == "" )
      return Set(input)

    rules.keySet.toList match {
      case Nil => Set(input)
      case head :: tail => {
        // Find index of the item
        val originalPos = input.indexOf(head)

        if( originalPos > -1 ) {
          // Before current match
          val starters = iterate(input.take(originalPos), rules - head)


          // Current match
          val current = rules(head).map( _.replacement )

          // After current match
          val rest = iterate(input.slice(originalPos + head.size, input.size), rules)

          // Combine all entries
          val combinations = for( part1 <- starters; part2 <- current; part3 <- rest ) yield part1 + part2 + part3

          return combinations.toSet
        } else {
          return iterate(input, rules - head)
        }
      }
    }
  }

  def main(args: Array[String]) {
    val filename = if (args.size > 0) args(0) else "/day19/input.txt"

    val (rules, initial) = loadData(filename)
    val rulesMap = rules.groupBy( _.original )

    val possibilities = iterate( initial, rulesMap )

    println( "-- Possibilities --" )
    for( possiblity <- possibilities ) {
      println( "  " + possiblity)
    }

    println("# possibilities: " + possibilities.size)

  }
}
