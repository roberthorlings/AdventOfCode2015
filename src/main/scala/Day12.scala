import org.json4s._
import org.json4s.native.JsonMethods._

object Day12 {
	def value(parsed: JValue): BigDecimal = parsed match {
		case JDouble(num) => BigDecimal(num)
		case JDecimal(num) => num
		case JInt(num) => BigDecimal(num)
		case JObject(obj) if(obj.exists((field) => field._2 == JString("red"))) => 0
    case JObject(obj) => obj.map((field) => value(field._2)).sum
		case JArray(obj) => obj.map((entry) => value(entry)).sum
		case _ => 0
	}

	def load(filename: String): JValue = {
	    parse(Utils.getInput(filename))
	}

  def main(args: Array[String]) {
    val filename = if (args.size > 0) args(0) else "/day12/input.txt"
    
    val sum = value(load(filename))
    
    println("Sum of all numbers: " + sum)
  }
}
