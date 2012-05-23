package bunny.el

object ElHelper {
	def isInteger(obj: Any) = (
	  obj.isInstanceOf[Int] ||
		obj.isInstanceOf[Long]
	)
	def asInteger(obj: Any): Long = obj match {
	  case i: Int => i.toLong
		case l: Long => l
		case _ => throw new IllegalArgumentException(
		  "cannot case [" + obj + "] to long")
	}
	def isNumber(obj: Any) = (
		obj.isInstanceOf[Int] ||
		obj.isInstanceOf[Long] ||
		obj.isInstanceOf[Float] || 
		obj.isInstanceOf[Double]
	)
	def asDouble(obj: Any): Double = obj match {
	  case i: Int => i.toDouble
		case l: Long => l.toDouble
		case f: Float => f.toDouble
		case d: Double => d
		case _ => throw new IllegalArgumentException(
		  "canot case [" + obj + "] to double")
	}
}
