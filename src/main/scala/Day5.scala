import scala.collection.mutable.Set
import scala.collection.mutable.MutableList

object Day5 {
  def findNiceStrings( filename: String ): Int = Utils.getStream(filename).getLines.foldLeft(0)((c: Int, word: String) => {
    val current = new SantaString(word)

    println( current )
    if( current.isNice2 )
      c + 1
    else
      c
  })

  def main(args: Array[String]) {
    val numNice = findNiceStrings("/day5/input.txt")
    println( "Total # nice strings " + numNice )
  }
}

class SantaString(val input: String) {
  // Part 1 of the assignment
  val dupsRegex = "(\\w)\\1+".r
  val combisRegex = "ab|cd|pq|xy".r

  def vowels = input.filter( "aeiou" contains _ ).size >= 3
  def dups   = ( dupsRegex findFirstIn input ).isDefined
  def combis = ( combisRegex findFirstIn input ).isDefined

  def isNice = vowels && dups && !combis

  // Part 2 of the assignment
  val pairsRegex = "(\\w{2}).*\\1".r
  val repeatsRegex = "(\\w).\\1".r

  def pairs = ( pairsRegex findFirstIn input ).isDefined
  def repeats = ( repeatsRegex findFirstIn input ).isDefined

  def isNice2 = pairs && repeats

  override def toString() = input + " -> " +
    (if(vowels) "vowels   " else "no vowels") + " " +
    (if(dups) "dups   " else "no dups") + " " +
    (if(combis) "combis   " else "no combis") + " " +
    (if(pairs) "pairs   " else "no pairs") + " " +
    (if(repeats) "repeats   " else "no repeats") + " " +
    " -> " +
    (if(isNice) "nice   " else "naughty") + " " +
    " / " +
    (if(isNice2) "nice2   " else "naughty2")
}
