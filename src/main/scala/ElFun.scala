package bunny.el

import ElHelper._

trait ElFun {
  def apply(args: List[Any]): Any
}
object SqrtFun extends ElFun {
  def apply(args: List[Any]): Any = {
	  if(args.length == 1) doApply(asDouble(args(0)))
		else throw new IllegalArgumentException("sqrt expect one argument")
	}
	private def doApply(value: Double): Double = scala.math.sqrt(value)
}
