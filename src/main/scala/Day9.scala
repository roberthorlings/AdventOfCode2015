object Day9 {
  val distanceMap = scala.collection.mutable.HashMap.empty[Edge, Int]
  val nodes = scala.collection.mutable.HashSet.empty[Node]

  type Node = String

  // Edge defines a single line in the node
  // Please note that for our purpose, node A - node B equals node B - node A
  class Edge(val node1: Node, val node2: Node) {
    override def equals(that: Any): Boolean =
      that match {
        case that: Edge => (that.node1 == this.node1 && that.node2 == this.node2) || (that.node1 == this.node2 && that.node2 == this.node1)
        case _ => false
      }

    override def hashCode: Int = 391 * node1.hashCode * node2.hashCode

  }

  def loadDistances(filename: String): Unit = {
    Utils.getStream(filename).getLines.foreach(parseLine(_))
  }

  def parseLine(line: String) {
    val distanceRegex = "([\\w ]+) to ([\\w ]+) = (\\d+)".r

    line match {
      case distanceRegex(node1, node2, distance) => {
        // Store distance
        distanceMap(new Edge(node1, node2)) = distance.toInt

        // Add nodes to the list of nodes
        nodes.add(node1)
        nodes.add(node2)
      }
      case _ => println("Invalid syntax in input file")
    }
  }

  def distance(edge: Edge): Int = distanceMap(edge)
  def distance(node1: Node, node2: Node): Int = distance(new Edge(node1, node2))

  def distance(nodes: List[Node]): Int = {
    return nodes match {
      case Nil => 0
      case head :: tail => tail match {
        case Nil => 0
        case second :: rest => distance(head, second) + distance(tail)
      }
    }
  }

  def bestDistance( method: (Iterator[Int]) => Int ): Int = {
    if( nodes.size == 0 )
      return 0

    // Create a list of all possible routes
    //   Discarding any reversions
    val routesToConsider = nodes.toList.permutations.filter((route: List[Node]) => route.head < route.last)

    // Inefficiently compute all distances and return the lowest
    method(routesToConsider.map(distance(_)))
  }


  def main(args: Array[String]) {
    val filename = if (args.size > 0) args(0) else "/day9/input.txt"
    val method = if (args.size > 1 && args(1) == "max" ) (d: Iterator[Int]) => d.max else (d: Iterator[Int]) => d.min

    loadDistances(filename)
    val best = bestDistance(method)

    println("Best distance: " + best)
  }
}
