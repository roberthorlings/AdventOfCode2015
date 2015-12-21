object Day18 {
  case class Coord(val row: Int, val col: Int ) {
    override def toString(): String = "" + row + "x" + col
  }

  type Rule = (GameOfLifeGrid, Coord, GameOfLifeGrid) => Unit

  class GameOfLifeGrid(val rows: Int, val cols: Int, val brokenCorners: Boolean){
    val grid = Array.ofDim[Boolean](rows, cols)

    def get(coord: Coord) = grid(coord.row)(coord.col)
    def set(coord: Coord, value: Boolean) = { grid(coord.row)(coord.col) = value }
    def initializeBrokenCorners() = {
      if( brokenCorners ) {
        // Make sure the corners are lit
        grid(0)(0) = true
        grid(0)(cols - 1) = true
        grid(rows - 1)(0) = true
        grid(rows - 1)(cols - 1) = true
      }
    }

    // Returns a set of neighbours
    def neighbours(current: Coord): Seq[Coord] = {
      for( row <- Math.max(0, current.row - 1) to Math.min(rows - 1, current.row + 1);
           col <- Math.max(0, current.col - 1) to Math.min(cols - 1, current.col + 1)
           if( row != current.row || col != current.col )
      ) yield new Coord(row, col)
    }

    // Checks whether a given position is a corner
    def isCorner(position: Coord) = position match {
      case Coord(row, col) if( row == 0        && col == 0 )         => true
      case Coord(row, col) if( row == 0        && col == cols- 1  )  => true
      case Coord(row, col) if( row == rows - 1 && col == 0 )         => true
      case Coord(row, col) if( row == rows - 1 && col == cols- 1  )  => true
      case _ => false
    }

    def iterate(rules: Seq[Rule], newGrid: GameOfLifeGrid): GameOfLifeGrid = {
      for( row <- 0 until rows; col <- 0 until cols; rule <- rules ) {
        rule(this, new Coord(row, col), newGrid)
      }
      newGrid
    }

    def iterate(): GameOfLifeGrid = {
      val newGrid = new GameOfLifeGrid(rows,cols, brokenCorners)
      val rules = List(
        (oldGrid: GameOfLifeGrid, position: Coord, newGrid: GameOfLifeGrid) => {
          // Handle special case for broken corners
          var newState = false

          if (brokenCorners && oldGrid.isCorner(position)) {
            newState = true
          } else {
            val num = oldGrid.numNeighboursOn(position)
            val currentState = oldGrid.get(position)

            newState = currentState match {
              case true if (num == 2 || num == 3) => true
              case false if (num == 3) => true
              case _ => false
            }
          }

          newGrid.set(position, newState)
        }
      )

      iterate(rules, newGrid)
    }

    def numNeighboursOn(position: Coord) = neighbours(position).map((neighbour: Coord) => get(neighbour)).filter(_ == true).size

    override def toString(): String = grid.map((row) => row.map {
      case true => '#'
      case false => '.'
    } mkString ).mkString( "\n" )

    def countLights() = grid.foldLeft(0)((sum: Int, row: Array[Boolean]) => {
      sum + row.filter(_ == true).size
    })

  }

  def loadData(filename: String, brokenCorners: Boolean): GameOfLifeGrid = {
    val lines = Utils.getStream(filename).getLines.toList
    val grid = new GameOfLifeGrid(lines.size, lines(0).size, brokenCorners)

    for( row <- 0 until lines.size; col <- 0 until lines(row).size  ) {
      grid.set(
        new Coord(row, col),
        lines(row)(col) match {
          case '#' => true
          case _ => false
        }
      )
    }

    grid

  }

  def main(args: Array[String]) {
    val filename = if (args.size > 0) args(0) else "/day18/input.txt"
    val iterations = if (args.size > 1) args(1).toInt else 1
    val brokenCorners = if( args.size > 2 && args(2) == "broken" ) true else false

    val initial = loadData(filename, brokenCorners)
    initial.initializeBrokenCorners()
    var currentGrid = initial

    for( i <- 0 until iterations ) {
      println( "Iteration " + i )
      currentGrid = currentGrid.iterate
    }

    println(currentGrid)
    println("# lights turned on: " + currentGrid.countLights())
  }
}
