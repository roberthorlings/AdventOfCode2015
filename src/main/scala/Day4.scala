import java.security.MessageDigest

object Day4 {
  val md5Instance = MessageDigest.getInstance("MD5")

  def md5(s: String): String = {
    md5Instance.digest(s.getBytes).map("%02x".format(_)).mkString
  }

  def firstMd5WithPrefix( input: String, prefix: String = "00000", max: Int = 50000000 ): Option[Int] = {
    var current: Int = 0;
    var currentHash: String = ""

    do {
      current = current + 1
      currentHash = md5(input + current)

      if( current % 10000 == 0 )
        print(".")
      if( current % 500000 == 0 )
        println("")

    } while( current <= max && !currentHash.startsWith(prefix) )

    // Either the hash is found or the max has been reached
    if( currentHash.startsWith(prefix)  )
      Some(current)
    else
      None
  }

  def main(args: Array[String]) {
    val input = "iwrupvqb"
    val prefix = "000000"
    firstMd5WithPrefix(input, prefix) match {
      case Some(number) => println( "Match found at index " + number + " -> " + md5( input + number) )
      case None => println( "No match found below 1e7")
    }

  }
}