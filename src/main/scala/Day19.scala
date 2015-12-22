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

  def indexesOf( input: String, search: String, start: Int = 0 ): List[Int] = {
    val pos = input.indexOf(search, start)

    if( pos == -1 )
      return List.empty[Int]
    else
      return pos :: indexesOf(input, search, pos + 1)
  }

  def iterate(input: Chromosome, rules: Map[String, Traversable[Rule]]): Set[Chromosome] = {
    if( input == "" )
      return Set(input)

    rules.keySet.toList.flatMap(
        (original) => indexesOf(input, original).flatMap(
          (index) => rules(original).map( (rule) => input.patch(index, rule.replacement, original.size) )
        )
      ).toSet
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
