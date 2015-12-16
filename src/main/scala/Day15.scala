object Day15 {
  class Ingredient(val name: String, val capacity: Long, val durability: Long, val flavor: Long, val texture: Long, val calories: Long) {
    override def toString(): String = name  + ": " + score()  + " " + List(capacity, durability, flavor, texture, calories)

    // Combining ingredients is possible as well
    def *(that: Long): Ingredient = new Ingredient("" + that + "x " + name, that * capacity, that * durability, that * flavor, that * texture, that * calories )
    def +(that: Ingredient): Ingredient = new Ingredient( name + " and " + that.name, capacity + that.capacity, durability + that.durability, flavor + that.flavor, texture + that.texture, calories + that.calories)

    // Compute score
    def score(): Long = if( capacity < 0 || durability < 0 || flavor < 0 || texture < 0 ) 0 else capacity * durability * flavor * texture
  }

  type Configuration = List[Long]

  def loadData(filename: String): List[Ingredient] = {
    Utils.getStream(filename).getLines.map(parseLine(_)).filter( _.isDefined ).map( _.get ).toList
  }

  def parseLine(line: String): Option[Ingredient] = {
    val regex = "(\\w+): capacity (-?\\d+), durability (-?\\d+), flavor (-?\\d+), texture (-?\\d+), calories (-?\\d+)".r.unanchored

    line match {
      case regex(name, capacity, durability, flavor, texture, calories) => Some(new Ingredient(name, capacity.toLong, durability.toLong, flavor.toLong, texture.toLong, calories.toLong))
      case _ => println("Invalid syntax in input file: " + line); None
    }
  }

  def getConfigurations(ingredients: Seq[Ingredient], total: Long): Seq[Configuration] = {
    ingredients match {
      case Nil => List()
      case head :: Nil => List( List(total) )
      case head :: tail => for( current <- 0L to total; otherIngredients <- getConfigurations( tail, total - current ) ) yield current :: otherIngredients
    }
  }

  def score(ingredients: Seq[Ingredient], configuration: Configuration): Ingredient = {
    ( ingredients zip configuration ).map( (tuple) => tuple._1 * tuple._2) match {
      case head :: tail => tail.foldLeft(head)((current, ingredient) => current + ingredient)
    }
  }

  def main(args: Array[String]) {
    val filename = if (args.size > 0) args(0) else "/day15/input.txt"
    val total = if (args.size > 1) args(1).toLong else 100
    val calories = if (args.size > 2) args(2).toLong else 0


    val ingredients = loadData(filename)
    val configurations = getConfigurations(ingredients, total)

    println("Computing best configuration out of " + configurations.size + " possibilities")
    if (calories > 0)
      println("  for # calories being exactly " + calories)

    for (ingredient <- ingredients)
      println("- " + ingredient)

    val scores = configurations map ((config) => score(ingredients, config))
    val validScores = if (calories == 0) scores else scores.filter(_.calories == calories)

    if (validScores.size == 0) {
      println("No valid configuration found for " + calories + " calories")
    } else {
      val best = validScores.maxBy(_.score)
      println("Best configuration: " + best)
    }
  }
}
