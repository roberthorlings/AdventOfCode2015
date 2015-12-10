import scala.collection.mutable.Set
import scala.collection.mutable.MutableList

object Day6 {

  def setupGrid[T]( filename: String, grid: Grid[T] ): Grid[T] = {
    Utils.getStream(filename).getLines.foreach(handleLine(_, grid))
    grid
  }

  def handleLine[T](line: String, grid: Grid[T]): Unit = {
    val regex = "(turn on|turn off|toggle) (\\d+),(\\d+) through (\\d+),(\\d+)".r

    line match {
      case regex(command, row1, col1, row2, col2) => {
        val rect = new Rect( row1.toInt, col1.toInt, row2.toInt, col2.toInt )
        println( " (" + command + ")")
        command match {
          case "turn on" => grid.turnOn(rect)
          case "turn off" => grid.turnOff(rect)
          case "toggle" => grid.toggle(rect)
        }
      }
    }
  }

  def main(args: Array[String]) {
    val rows = 1000
    val cols = 1000
    val filename = if( args.size > 0 ) args(0) else "/day6/input.txt"
    val grid = if( args.size > 1 && args(1) == "integer") new IntegerGrid(rows,cols) else new BinaryGrid(rows, cols)

    setupGrid(filename, grid)
    println( "" + grid )
  }
}

abstract class Grid[T: ClassManifest](val rows: Int, val cols: Int) {
  val grid = Array.ofDim[T](rows, cols)

  def update(rect: Rect, updateMethod: (T) => T ): Unit = {
    val rectRows = rect.rows()
    val rectCols = rect.cols()

    println("  Updating " + rect + " -> " + updateMethod)
    println("    - " + rectCols.size + "x" + rectRows.size)
    for( col <- rectCols; row <- rectRows )
      grid(row)(col) = updateMethod(grid(row)(col))
  }

  def turnOn(rect: Rect)
  def turnOff(rect: Rect)
  def toggle(rect: Rect)
}

class BinaryGrid(rows: Int, cols: Int) extends Grid[Boolean](rows,cols){
  override def turnOn(rect: Rect) = update(rect, (_: Boolean) => { true } )
  override def turnOff(rect: Rect) = update(rect, (_: Boolean) => { false } )
  override def toggle(rect: Rect) = update(rect, (current: Boolean) => { !current } )

  def countOn() = grid.foldLeft(0)((sum: Int, row: Array[Boolean]) => {
    sum + row.filter(_ == true).size
  })

  override def toString(): String = "BinaryGrid " + rows + "x" + cols + " -> " + countOn() + " lights are lit"
}

class IntegerGrid(rows: Int, cols: Int) extends Grid[Int](rows,cols){
  override def turnOn(rect: Rect) = update(rect, (current: Int) => { current + 1 } )
  override def turnOff(rect: Rect) = update(rect, (current: Int) => { Math.max( current - 1, 0 ) } )
  override def toggle(rect: Rect) = update(rect, (current: Int) => { current + 2 } )

  def sumBrightness() = grid.foldLeft(0)((sum: Int, row: Array[Int]) => {
    sum + row.sum
  })

  override def toString(): String = "Integer Grid " + rows + "x" + cols + " -> " + sumBrightness() + " total brightness"
}

class Rect(val corner1: Coord, val corner2: Coord) {
  def this(row1: Int, col1: Int, row2: Int, col2: Int) = this(new Coord(row1,col1), new Coord(row2, col2))
  def rows(): Range = range(corner1.row, corner2.row)
  def cols(): Range = range(corner1.col, corner2.col)

  def range(idx1: Int, idx2: Int): Range = if( idx1 <= idx2 ) idx1 to idx2 else idx2 to idx1

  override def toString(): String = "Rect " + corner1 + " - " + corner2
}

class Coord(val row: Int, val col: Int ) {
  override def toString(): String = "" + row + "x" + col
}
