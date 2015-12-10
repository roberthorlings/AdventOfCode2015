
object Day2 {
  // Read the file contents
  def parseLine( line: String ): Box = {
    val dimensions = line.split( "x" ).map( _.toInt )
    return new Box(dimensions(0), dimensions(1), dimensions(2))
  }

  def computeOnBoxes(filename: String, computation: ((Box) => Int)): Int = {
    Utils.getStream(filename)
      .getLines
      .map( parseLine(_) )
      .foldLeft(0)((paper: Int, box: Box) => paper + computation(box) )
  }
  def getWrappingPaper(filename: String): Int = {
    computeOnBoxes(filename, (box: Box) => box.wrappingPaper )
  }

  def getRibbon(filename: String): Int = {
    computeOnBoxes(filename, (box: Box) => box.ribbon )
  }


  def main(args: Array[String]) {
    println( "Total wrapping paper " + getWrappingPaper("/day2/input.txt") )
    println( "Total ribbon         " + getRibbon("/day2/input.txt") )
  }

}

class Box(w: Int, l: Int, h: Int) {
  def surface(): Int = 2 * ( w * l + w * h + h * l )
  def volume(): Int = w * h * l

  def extraPaper(): Int = Array( w * l, w * h, h * l ).min
  def wrappingPaper(): Int = surface + extraPaper

  def ribbonBow(): Int = 2 * ( (w + h + l) - Array( w, h, l ).max)
  def ribbon(): Int = volume + ribbonBow
}