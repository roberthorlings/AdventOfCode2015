object Day13 {
  val happinessMap = scala.collection.mutable.HashMap.empty[Edge, Int]
  val persons = scala.collection.mutable.HashSet.empty[Person]

  type Person = String
  type Configuration = List[Person]

  // Edge defines a single line in the node
  // Please note that for our purpose, node A - node B equals node B - node A
  class Edge(val node1: Person, val node2: Person) {
    override def equals(that: Any): Boolean =
      that match {
        case that: Edge => (that.node1 == this.node1 && that.node2 == this.node2) || (that.node1 == this.node2 && that.node2 == this.node1)
        case _ => false
      }

    override def hashCode: Int = 391 * node1.hashCode * node2.hashCode

    override def toString(): String = node1 + " - " + node2
  }

  def loadMatrix(filename: String): Unit = {
    Utils.getStream(filename).getLines.foreach(parseLine(_))
  }

  def parseLine(line: String) {
    val happinessRegex = "([\\w ]+) would (gain|lose) (\\d+) happiness units by sitting next to ([\\w ]+)".r.unanchored

    line match {
      case happinessRegex(node1, gainOrLose, value, node2) => {
        val newValue = if (gainOrLose == "gain") value.toInt else -value.toInt

        // As both persons contribute to the value of the happiness of an edge
        // we should see if a value already exists
        val currentEdge = new Edge(node1, node2)
        val currentValue = happinessMap.getOrElse(currentEdge, 0)

        // Set new value
        happinessMap(currentEdge) = currentValue + newValue

        // Add nodes to the list of nodes
        persons.add(node1)
        persons.add(node2)
      }
      case _ => println("Invalid syntax in input file: " + line)
    }
  }

  def includeMyself(): Unit = {
    persons.add( "Robert" )
  }

  def happiness(edge: Edge): Int = happinessMap.getOrElse(edge, 0)

  def happiness(node1: Person, node2: Person): Int = happiness(new Edge(node1, node2))

  def happiness(nodes: Configuration): Int = linearHappiness(nodes) + wrapAroundHappiness(nodes)

  def linearHappiness(nodes: Configuration): Int = {
    return nodes match {
      case Nil => 0
      case head :: Nil => 0
      case head :: second :: tail => happiness(head, second) + linearHappiness( second :: tail)
    }
  }

  def wrapAroundHappiness(nodes: Configuration): Int = {
    if (nodes.size >= 2)
      happiness(nodes.head, nodes.last)
    else
      0
  }

  def bestConfiguration(): (Configuration, Int) = {
    allConfigurations.maxBy(_._2)
  }

  def allConfigurations(): Map[Configuration, Int] = {
    if (persons.size == 0)
      return Map.empty

    // Create a list of all possible configurations
    //   Discarding any reversions
    //   Always starting with a single person (as it is circular)
    val configurationsToConsider =
      persons.tail.toList
        .permutations
        .filter((route: List[Person]) => route.head < route.last)
        .map( persons.head :: _ )

    // Inefficiently compute all distances and return the lowest
    val map = scala.collection.mutable.HashMap.empty[Configuration, Int]
    for( config <- configurationsToConsider )
      map(config) = happiness(config)

    // Convert to immutable map
    map.toMap
  }

  def main(args: Array[String]) {
    val filename = if (args.size > 0) args(0) else "/day9/input.txt"
    val shouldIncludeMyself = (args.size > 1 && args(1) == "include")

    loadMatrix(filename)

    if( shouldIncludeMyself )
      includeMyself()

    val best = bestConfiguration()

    println("Best configuration happiness: " + best)
  }
}
