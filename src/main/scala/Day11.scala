object Day11 {
  implicit class StringImprovements(val s: String) {
    def isValid(): Boolean = {
      // Passwords must include one increasing straight of at least three letters, like abc, bcd, cde, and so on, up to xyz. They cannot skip letters; abd doesn't count.
      if( !s.hasStraight )
        return false

      // Passwords may not contain the letters i, o, or l, as these letters can be mistaken for other characters and are therefore confusing.
      if( !s.onlyValidCharacters() )
        return false

      // Passwords must contain at least two different, non-overlapping pairs of letters, like aa, bb, or zz.
      if( !s.hasPairs )
        return false

      return true
    }

    def onlyValidCharacters(): Boolean = !s.exists( "iol" contains _ )
    def hasStraight(): Boolean = s.length > 2 && s.sliding(3).exists( (triple) => triple(0) + 1 == triple(1) && triple(1) + 1 == triple(2) )
    def hasPairs(): Boolean = s.length > 1 && s.sliding(2).filter( (pair) => pair(0) == pair(1) ).toList.distinct.size > 1
  }

  val firstCharacter = 'a'
  val lastCharacter = 'z'

	def nextPassword(current: String, full: Boolean = true): String = {
		if( current == "" )
			return firstCharacter.toString

    // Special cases to speed up the search
    // Check for a i, o or l in the string
    val invalidCharIdx = current.indexWhere( "iol" contains _ )
    if( invalidCharIdx >= 0 ) {
      return current.slice( 0, invalidCharIdx ) + nextChar(current(invalidCharIdx)) +  ( firstCharacter.toString * (current.size - invalidCharIdx - 1 ))
    }

    // If there the number of pairs is not sufficient, make sure the last two characters are the same
    val last = current.last
    if( full && !current.hasPairs() && current.length > 1) {
      val second = current(current.length - 2)

      if( last < second )
        return current.substring(0, current.length - 2) + second + second
      else if( second == lastCharacter )
        return nextPassword( current.substring(0, current.length - 2 ), false ) + firstCharacter + firstCharacter
      else
        return current.substring(0, current.length - 2 ) + nextChar(second) + nextChar(second)

    }


		// Either increase the last character or set it to 'a' and continue
    val start = current.substring(0, current.length - 1)

    if( last == lastCharacter )
      nextPassword(start, false ) + firstCharacter
    else
      start + nextChar(current.last)
	}

  def nextChar(c: Char): Char = if( c == lastCharacter ) firstCharacter else (c + 1).toChar

  def main(args: Array[String]) {
    val input = if (args.size > 0) args(0) else "abcdefgh"
    val max = if (args.size > 1) args(1).toInt else 1000000
    val iterations = if (args.size > 2) args(2).toInt else 1

    var current = input
    for( i <- 0 until iterations ) {
      println( "Iteration " + i )
      var j = 0
      do {
        current = nextPassword(current)
        j = j + 1

        if( j % 1000 == 0 )
          println( "%5d".format(j) + " Current pw: " + current )
      } while( !current.isValid && j < max)
    }

    println( "Input:   " + input )
    println( "  Valid:  " + input.isValid() + " / " + input.hasStraight() + " / " + input.onlyValidCharacters() + " / " + input.hasPairs() )
    println( "Next:  " + current )
    println( "  Valid:  " + current.isValid() + " / " + current.hasStraight() + " / " + current.onlyValidCharacters() + " / " + current.hasPairs() )
  }
}
