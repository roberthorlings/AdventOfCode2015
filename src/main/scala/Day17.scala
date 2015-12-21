object Day17 {
  type Configuration = List[Int]

  def loadData(filename: String): List[Int] = {
    Utils.getStream(filename).getLines.map(_.toInt).toList.sorted
  }

  def getConfigurations(containers: List[Int], total: Int): Seq[Configuration] = {
    val configurations = scala.collection.mutable.ListBuffer.empty[Configuration]

    if( containers.size == 0 )
      return configurations

    for( i <- 0 until containers.size ) {
      val current = containers(i)
      val rest = containers.slice( i + 1, containers.size)

      // Return if the current container is too large for the amount to search
      // If so, we can stop this loop, as the containers are sorted
      if( current > total )
        return configurations
      else if( current == total )
        configurations += List(current)
      else
        configurations ++= getConfigurations(rest, total - current).map( (subConfig) => current :: subConfig )
    }

    configurations.toList
  }

  def main(args: Array[String]) {
    val filename = if (args.size > 0) args(0) else "/day17/input.txt"
    val total = if (args.size > 1) args(1).toInt else 25

    val containers = loadData(filename)
    val configurations = getConfigurations(containers, total)

    for( config <- configurations )
      println( "   " + config )

    println( "# configurations: " + configurations.size )

    // Part 2 exercise
    val minSize = configurations.map( _.size ).min
    val minConfigs = configurations.filter( _.size == minSize )

    println( "# configurations with minimum size (" + minSize + "): " + minConfigs.size)
  }
}
