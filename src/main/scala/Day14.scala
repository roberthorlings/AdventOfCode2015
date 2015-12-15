object Day14 {
  class Reindeer(val name: String, val speed: Int, val timeFlying: Int, val timeResting: Int) {

    def timeFullCycle = timeFlying + timeResting
    def numFullCycles(numSeconds: Int): Int = Math.floor( numSeconds / timeFullCycle ).toInt

    // Run a simulation for the given number of seconds
    def simulate(numSeconds: Int): Int = {
      // Number of full cycles + remaining part
      // The remaining cycle starts with flying, so take
      // the remaining number of seconds (max = timeFlying) and
      numFullCycles(numSeconds) * speed * timeFlying + Math.min( numSeconds % timeFullCycle, timeFlying ) * speed
    }

    override def toString(): String = name
  }

  def loadData(filename: String): Iterator[Reindeer] = {
    Utils.getStream(filename).getLines.map(parseLine(_)).filter( _.isDefined ).map( _.get )
  }

  def parseLine(line: String): Option[Reindeer] = {
    val reindeerRegex = "([\\w ]+) can fly (\\d+) km/s for (\\d+) seconds, but then must rest for (\\d+) seconds".r.unanchored

    line match {
      case reindeerRegex(name, speed, timeFlying, timeResting) => Some(new Reindeer(name, speed.toInt, timeFlying.toInt, timeResting.toInt))
      case _ => println("Invalid syntax in input file: " + line); None
    }
  }

  def simulate(reindeers: Iterator[Reindeer], numSeconds: Int): Map[Reindeer,Int] = {
    val map = scala.collection.mutable.HashMap.empty[Reindeer, Int]
    for( reindeer <- reindeers )
      map(reindeer) = reindeer.simulate(numSeconds)

    map.toMap
  }

  def main(args: Array[String]) {
    val filename = if (args.size > 0) args(0) else "/day14/input.txt"
    val duration = if (args.size > 1) args(1).toInt else 2503

    val reindeers = loadData(filename)
    val simulated = simulate(reindeers, duration)
    val best = simulated.maxBy(_._2)

    for( simulation <- simulated )
      println(" - " + simulation)

    println("Best reindeer after " + duration + " seconds: " + best)
  }
}
