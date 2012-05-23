package bunny.el

import bunny.BunnyException
import ElHelper._

trait ElEvaluator {
  def apply(expr: String, ctx: Map[String, Any] = Map()): Any
  def apply(expr: ElExpr, ctx: Map[String, Any]): Any
}

class DefaultElEvaluator(parser: ElParser) extends ElEvaluator {
  def apply(expr: String, ctx: Map[String, Any] = Map()): Any = apply(parser(expr), ctx)
  def apply(expr: ElExpr, ctx: Map[String, Any]): Any = expr match {
    case IntegerLiteral(value) => value
		case FloatLiteral(value) => value
		case StringLiteral(value) => value
		case TrueLiteral => true
		case FalseLiteral => false
		case NullLiteral => null
		case BinaryExpr(left, op, right) => {
		  val lVal = apply(left, ctx)
			if(lVal == true && op == OrOper) true
			else if(lVal == false && op == AndOper) false
			else op(lVal, apply(right, ctx))
		}
		case MultiOpExpr(left, rest) => rest.foldLeft(apply(left, ctx)){
		  (res, cur) => cur._1(res, apply(cur._2, ctx))
		}
		case UnaryExpr(ops, value) => ops.reverse.foldLeft(apply(value, ctx)){
		  (res, cur) => cur(res)
		}
		case Identifier(name) => findVar(name, ctx)
		case Value(pfx, rest) => rest.foldLeft(apply(pfx, ctx)){
		  (res, cur) => cur match {
			  case PropertyName(id) => getProp(res, id.name)
				case PropertyExpr(expr) => getProp(res, apply(expr, ctx))
			}
		}
		case FunctionInvocation(id, args) => findFunc(id.name) match {
		  case Some(func) => func(args.map(x => apply(x, ctx)))
      // TODO rename to function not found exception
			case _ => throw new IllegalArgumentException("function [" + id.name + "] not found")
		}
    // TODO rename to unsuported el expression expection
		case _ => throw new UnsupportedOperationException
	}
	private final val funcDict = Map(
	  "sqrt" -> SqrtFun
	)
	private def findFunc(name: String): Option[ElFun] = funcDict.get(name)
  class VariableNotFoundException(msg: String) extends BunnyException(msg)
	private def findVar(name: String, ctx: Map[String, Any]): Any = ctx.get(name) match {
		case Some(value) => value
		case _ => throw new VariableNotFoundException("variable [" + name + "] not found")
	}
	private def getProp(obj: Any, key: Any): Any = {
		def asInt: Int = key match {
		  case i: Int => i
			case l: Long => l.toInt
			case _ => throw new IllegalArgumentException("cannot cast [" + key + "] to int")
		}
		def fromArray: Any = obj.asInstanceOf[Array[_]](asInt)
	  def fromSeq: Any = obj.asInstanceOf[Seq[Any]](asInt)
		def fromMap: Any = obj.asInstanceOf[Map[String, Any]].get(key.asInstanceOf[String]).get
	  def fromBean: Any = {
		  val name = key.asInstanceOf[String]
			val caped = name.head.toUpper + name.tail
			val objCls = obj.getClass
			val methods = objCls.getMethods.filter(_.getParameterTypes.length == 0).map(m => (m.getName, m)).toMap
			List(name, "is" + caped, "get" + caped).find(mName => methods.contains(mName)).map{mName =>
				methods.get(mName).get.invoke(obj)
			} match {
				case Some(value) => value
				case _ => throw new IllegalArgumentException("no getter method for property [" + name + "] in class [" + objCls + "]")
			}
		}
		try {
			obj match {
				case arr: Array[_] => fromArray
				case seq: Seq[_] => fromSeq 
				case map: Map[_, _] if key.isInstanceOf[String] => fromMap 
			}
		}catch{ 
		  case _ => fromBean
		}
	}
}
