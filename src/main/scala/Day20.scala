/**
  * Created by robert on 6-1-16.
  */
object Day20 {
  def divisors(num: Int) : Set[Int] = num match {
    case 1 => Set(1)
    case _ => {
      // Find all divisors upto the square root of the number
      val lowDivisors = List(1) ++ ( 2 to Math.floor(Math.sqrt(num)).toInt ).filter( num % _ == 0 )

      // From those divisors, also take the 'complement' (num / divisor), which is a divisor as well
      ( lowDivisors ++ lowDivisors.map( num / _ ) ).toSet
    }
  }

  def main(args: Array[String]) {
    val input = if (args.size > 0) args(0).toLong else 3400000
    val max = if (args.size > 1) args(1).toInt else 1000000

    var house = 1
    var found = false
    while( house < max && !found ) {
      // The number of presents is 10x the sum of all divisors
      // However, we don't multiply by 10
      val numPresents = divisors(house).sum

      if( house % 100 == 0 ) {
        print( "." )
      }

      if( house % 10000 == 0 ) {
        println( " " + house + " -> " + numPresents )
      }

      if( numPresents >= input ) {
        println("")
        println("House " + house + ": " + numPresents )
        println("House " + house + " is the first house to have more than " + input + " presents")
        found = true
      }

      house = house + 1
    }

    println("")
  }

}
