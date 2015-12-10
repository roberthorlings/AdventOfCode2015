
object Day1 {
  // Read the file contents
  def computeFloor( filename: String, initialFloor: Int = 0 ) = {
    // Compute the final floor
    Utils.getInput(filename).foldLeft(initialFloor)(
      (currentFloor: Int, character: Char) => currentFloor + move(character)
    )
  }

  def move(character: Char) = character match {
    case '(' => 1
    case ')' => -1
    case _ => 0
  }

  def getPositionToEnterBasement(filename: String, initialFloor: Int = 0 ): Int = {
    var floor = initialFloor
    var currentCharacter = 1

    for( character <- Utils.getInput(filename) ) {
      floor += move(character)

      if( floor < 0 )
        return currentCharacter

      currentCharacter = currentCharacter + 1
    }

    return -1
  }

  def main(args: Array[String]) {
    println( "Final floor " + computeFloor("/day1/input.txt") )
    println( "Character to reach  basement " + getPositionToEnterBasement("/day1/input.txt") )
  }

}