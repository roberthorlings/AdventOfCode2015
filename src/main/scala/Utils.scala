object Utils {
  def getStream(filename: String) = scala.io.Source.fromInputStream(getClass.getResourceAsStream(filename))
  def getInput(filename: String) = getStream(filename).mkString
}