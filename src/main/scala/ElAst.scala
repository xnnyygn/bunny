package bunny.el

trait ElExpr
sealed class BooleanLiteral extends ElExpr
case object TrueLiteral extends BooleanLiteral
case object FalseLiteral extends BooleanLiteral

// number literal
trait NumberLiteral extends ElExpr
case class IntegerLiteral(value: Long) extends NumberLiteral
case class FloatLiteral(value: Double) extends NumberLiteral
object NumberLiteral {
	private final val integerRegex = """^\d+$""".r
  def apply(str: String): NumberLiteral = {
	  if(integerRegex.findFirstIn(str).isDefined) IntegerLiteral(str.toLong)
		else FloatLiteral(str.toDouble)
	}
}

case class StringLiteral(value: String) extends ElExpr
case object NullLiteral extends ElExpr

trait UnaryOper {
  def apply(value: Any): Any
}
case object NegativeOper extends UnaryOper {
  def apply(value: Any): Any = {
	  import ElHelper._
	  if(isInteger(value)) -asInteger(value)
		else if(isNumber(value)) -asDouble(value)
		else throw new IllegalArgumentException("negative must apply on number")
	}
}
case object NotOper extends UnaryOper {
  def apply(value: Any): Any = {
	  if(value.isInstanceOf[Boolean]) !value.asInstanceOf[Boolean]
		else throw new IllegalArgumentException("not must apply on boolean")
	}
}
case object EmptyOper extends UnaryOper {
  def apply(value: Any): Any = {
	  return (value == null || 
		  (value.isInstanceOf[Array[_]] && value.asInstanceOf[Array[Any]].length == 0))
	}
}
case class UnaryExpr(ops: List[UnaryOper], value: ElExpr) extends ElExpr

trait BinaryOper {
  def apply(left: Any, right: Any): Any
}
object BinaryOper {
  def apply(op: String): BinaryOper = op match {
	  case "+" => PlusOper
		case "-" => MinusOper
		case "*" => MultiplyOper
		case "/" => DivideOper
		case "&&" => AndOper
		case "||" => OrOper
		case ">" => GreaterThanOper
		case ">=" => GreaterThanAndEqualOper
		case "<" => LessThanOper
		case "<=" => LessThanAndEqualOper
		case "==" => EqualOper
		case "!=" => NotEqualOper
	}
	def isNumber(obj: Any) = (
		obj.isInstanceOf[Int] ||
		obj.isInstanceOf[Long] ||
		obj.isInstanceOf[Float] || 
		obj.isInstanceOf[Double]
	)
	def asInteger(obj: Any) = obj match {
		case i: Int => i.toLong
		case l: Long => l
		case _ => throw new IllegalArgumentException(
		  "cannot cast [" + obj + "] to integer(long)") 
	}
	def asDouble(obj: Any) = obj match {
	  case i: Int => i.toDouble
		case l: Long => l.toDouble
		case f: Float => f.toDouble
		case d: Double => d
		case _ => throw new IllegalArgumentException(
		  "canot case [" + obj + "] to double")
	}
}
trait ArithOper extends BinaryOper {
  def apply(left: Any, right: Any): Any = {
	  import BinaryOper._
		def isInteger(obj: Any): Boolean = obj.isInstanceOf[Int] || obj.isInstanceOf[Long]
	  if(isInteger(left) && isInteger(right)) applyToInteger(asInteger(left), asInteger(right))
		else if(isNumber(left) && isNumber(right)) applyToOther(asDouble(left), asDouble(right))
		else throw new IllegalArgumentException("arith operator must apply on number")
	}
	protected def applyToInteger(left: Long, right: Long): Long 
	protected def applyToOther(left: Double, right: Double): Double
}
case object PlusOper extends ArithOper {
  def applyToInteger(left: Long, right: Long) = left + right
	def applyToOther(left: Double, right: Double) = left + right
}
    
case object MinusOper extends ArithOper {
  def applyToInteger(left: Long, right: Long) = left - right
	def applyToOther(left: Double, right: Double) = left - right
}
case object MultiplyOper extends ArithOper {
  def applyToInteger(left: Long, right: Long) = left * right
	def applyToOther(left: Double, right: Double) = left * right
}
case object DivideOper extends ArithOper {
  def applyToInteger(left: Long, right: Long) = left / right
	def applyToOther(left: Double, right: Double) = left / right
}

trait BooleanOper extends BinaryOper {
  def apply(left: Any, right: Any): Any = {
	  def isBool(obj: Any) = obj.isInstanceOf[Boolean]
		def asBool(obj: Any) = obj.asInstanceOf[Boolean]
		if(isBool(left) && isBool(right)) doApply(asBool(left), asBool(right))
		else throw new IllegalArgumentException("boolean operation must apply on boolean value")
	}
	protected def doApply(left: Boolean, right: Boolean): Boolean
}
case object AndOper extends BooleanOper {
  def doApply(left: Boolean, right: Boolean) = { left && right }
}
case object OrOper extends BooleanOper {
  def doApply(left: Boolean, right: Boolean) = { left || right }
}

trait CompareOper extends BinaryOper {
  def apply(left: Any, right: Any): Any = {
		import ElHelper._
		def comparable(obj: Any) = obj.isInstanceOf[Comparable[_]]
	  if(isNumber(left) && isNumber(right)) 
		  applyToNumber(asDouble(left), asDouble(right))
		else if(comparable(left) && comparable(right))
		  compareObj(left.asInstanceOf[Comparable[Any]].compareTo(right))
		else throw new IllegalArgumentException(
		  "compare operation must apply on number or other object implment comparable")
	}
	protected def applyToNumber(left: Double, right: Double): Boolean
	protected def compareObj(result: Int): Boolean
}
case object GreaterThanOper extends CompareOper {
  def applyToNumber(left: Double, right: Double) = left > right
	def compareObj(result: Int) = result == 1
}
case object LessThanOper extends CompareOper {
  def applyToNumber(left: Double, right: Double) = left < right
	def compareObj(result: Int) = result == -1
}
case object GreaterThanAndEqualOper extends CompareOper {
  def applyToNumber(left: Double, right: Double) = left >= right
	def compareObj(result: Int) = result >= 0
}
case object LessThanAndEqualOper extends CompareOper {
  def applyToNumber(left: Double, right: Double) = left <= right
	def compareObj(result: Int) = result <= 0
}
case object EqualOper extends CompareOper {
  def applyToNumber(left: Double, right: Double) = left == right
	def compareObj(result: Int) = result == 0
}
case object NotEqualOper extends CompareOper {
  def applyToNumber(left: Double, right: Double) = left != right
	def compareObj(result: Int) = result != 0
}

case class BinaryExpr(left: ElExpr, op: BinaryOper, right: ElExpr) extends ElExpr 
case class MultiOpExpr(left: ElExpr, rest: List[(BinaryOper, ElExpr)]) extends ElExpr

sealed trait PropertyKey extends ElExpr
case class PropertyExpr(expr: ElExpr) extends PropertyKey
case class PropertyName(id: Identifier) extends PropertyKey

case class Value(prefix: ElExpr, propChain: List[PropertyKey]) extends ElExpr {
  override def toString = propChain match {
	  case Nil => "Value(" + prefix + ")"
		case _ => "Value(" + prefix + ", " + propChain + ")"
	}
}
case class Identifier(name: String) extends ElExpr
case class FunctionInvocation(id: Identifier, arguments: List[ElExpr]) extends ElExpr
