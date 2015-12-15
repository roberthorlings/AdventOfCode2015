object Day14 {
  class Reindeer(val name: String, val speed: Int, val timeFlying: Int, val timeResting: Int, var distance: Int = 0, var score: Int = 0) {
    def timeFullCycle = timeFlying + timeResting
    def numFullCycles(numSeconds: Int): Int = Math.floor( numSeconds / timeFullCycle ).toInt

    // Run a simulation for the given number of seconds
    def getDistance(numSeconds: Int): Int = {
      // Number of full cycles + remaining part
      // The remaining cycle starts with flying, so take
      // the remaining number of seconds (max = timeFlying) and
      numFullCycles(numSeconds) * speed * timeFlying + Math.min( numSeconds % timeFullCycle, timeFlying ) * speed
    }

    // Simulate a single second, and increase the distance when not running
    def simulateSecond( time: Int ): Unit = {
      if( time % timeFullCycle < timeFlying )
        distance += speed
    }

    // Reset the distance and score
    def reset(): Unit = {
      distance = 0
      score = 0
    }

    def addBonus(): Unit = {
      score = score + 1
    }

    override def toString(): String = "" + name + " / " + distance + " / " + score
  }

  def loadData(filename: String): List[Reindeer] = {
    Utils.getStream(filename).getLines.map(parseLine(_)).filter( _.isDefined ).map( _.get ).toList
  }

  def parseLine(line: String): Option[Reindeer] = {
    val reindeerRegex = "([\\w ]+) can fly (\\d+) km/s for (\\d+) seconds, but then must rest for (\\d+) seconds".r.unanchored

    line match {
      case reindeerRegex(name, speed, timeFlying, timeResting) => Some(new Reindeer(name, speed.toInt, timeFlying.toInt, timeResting.toInt))
      case _ => println("Invalid syntax in input file: " + line); None
    }
  }

  def getDistance(reindeers: Iterator[Reindeer], numSeconds: Int): Map[Reindeer,Int] = {
    val map = scala.collection.mutable.HashMap.empty[Reindeer, Int]
    for( reindeer <- reindeers )
      map(reindeer) = reindeer.getDistance(numSeconds)

    map.toMap
  }

  def simulate(reindeers: List[Reindeer], numSeconds: Int): List[Reindeer] = {
    // Reset all reindeers to their initial state
    reindeers.foreach( _.reset )

    for( time <- 0 until numSeconds ) {
      reindeers.foreach( _.simulateSecond(time) )

      // Add a bonus for each reindeer being the best
      val bestScore = reindeers.map(_.distance).max
      reindeers.filter(_.distance == bestScore).foreach( _.addBonus )
    }

    reindeers
  }


  def main(args: Array[String]) {
    val filename = if (args.size > 0) args(0) else "/day14/input.txt"
    val duration = if (args.size > 1) args(1).toInt else 2503

    val reindeers = loadData(filename)
    val simulated = simulate(reindeers, duration)
    val best = simulated.maxBy(_.score)

    for( simulation <- simulated )
      println(" - " + simulation)

    println("Best reindeer after " + duration + " seconds: " + best)
  }
}
