import scala.collection.mutable.Set
import scala.collection.mutable.MutableList

object Day3 {
  def visit( filename: String, num: Int = 2 ): Set[Loc] = {
    val initial = new Loc(0,0)
    val visited: Set[Loc] = Set()

    // Initialize delivery men at the initial position
    val deliveryMen = MutableList.fill(num)(initial)

    // Keep track of whose turn it is
    var currentDeliveryMan = 0

    // Initial position also counts as one
    visited.add(initial)

    for( character <- Utils.getInput(filename) ) {
      val next = deliveryMen(currentDeliveryMan) + move(character)

      println( "" + currentDeliveryMan + " - " + character + " - " + next.x + ", " + next.y)

      // Update pointers
      deliveryMen(currentDeliveryMan) = next
      currentDeliveryMan = ( currentDeliveryMan + 1 ) % num

      // Mark this item as visited
      visited.add( next )
    }

    return visited
  }


  def move(character: Char) = character match {
    case '^' => LocationConstants.UP
    case '>' => LocationConstants.RIGHT
    case '<' => LocationConstants.LEFT
    case 'v' => LocationConstants.DOWN
  }

  def main(args: Array[String]) {
    val visited = visit("/day3/input.txt", 1)
    println( "Total # addresses visited " + visited.size )

    val visited2 = visit("/day3/input.txt", 2 )
    println( "Total # addresses visited by 2 persons: " + visited2.size )

  }
}

class Loc(val x: Int, val y: Int) {
  def +(that: Loc) = new Loc( this.x + that.x, this.y + that.y)

  override def equals(that: Any): Boolean =
    that match {
      case other: Loc => this.x == other.x && this.y == other.y
      case _ => false
    }

  override def hashCode:Int = {
    val prime = 11813
    return prime * x + y;
  }

}

object LocationConstants {
  val UP = new Loc(0, 1)
  val DOWN = new Loc(0, -1)
  val LEFT = new Loc(-1, 0)
  val RIGHT = new Loc(1, 0)
}