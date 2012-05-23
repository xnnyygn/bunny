package bunny.el

import bunny.BunnyException

trait ElParser {
  def apply(expr: String): ElExpr
}

class ElParseException(msg: String) extends BunnyException(msg)

import scala.util.parsing.combinator._

class JavaTokenElParser extends ElParser with JavaTokenParsers {
	def elExpr: Parser[ElExpr] = "${"~>elExprInner<~"}"
  def elExprInner:Parser[ElExpr] = expr | nonLiteralValuePrefix~rep(valueSuffix) ^^ {case pfx~sfxes => Value(pfx, sfxes)}
  def identifier:Parser[Identifier] = """\w[\w\d$_]*""".r ^^ (x => Identifier(x)) | failure("illegal start of identifier")
  def nonLiteralValuePrefix:Parser[ElExpr]= "("~>expr<~")" | functionInvocation | identifier 
  def valueSuffix:Parser[PropertyKey] = "."~>identifier ^^ (x => PropertyName(x)) | "["~>expr<~"]" ^^ (x => PropertyExpr(x))
  def functionInvocation:Parser[FunctionInvocation] = identifier~functionArguments ^^ {case id~args => FunctionInvocation(id, args)}
	private def functionArguments: Parser[List[ElExpr]] = "("~>repsep(expr, ",")<~")"
	def expr: Parser[ElExpr] = boolExpr1~"||"~boolExpr1 ^^ {
	  case l~m~r => new BinaryExpr(l, OrOper, r)} | boolExpr1
	def boolExpr1: Parser[ElExpr] = boolExpr2~"&&"~boolExpr2 ^^ {
	  case l~m~r => new BinaryExpr(l, AndOper, r)} | boolExpr2
	def boolExpr2: Parser[ElExpr] = compExpr1~("==" | "!=")~compExpr1 ^^ {
	  case l~m~r => BinaryExpr(l, BinaryOper(m), r)} | compExpr1
	def compExpr1: Parser[ElExpr] = compExpr2~("<" | ">" | "<=" | ">=")~compExpr2 ^^ {
	  case l~m~r => BinaryExpr(l, BinaryOper(m), r)} | compExpr2
	def compExpr2: Parser[ElExpr] = numExpr1~rep(("+" | "-")~numExpr1 ^^ opNumTuple) ^^ arithSeq
  private def opNumTuple: PartialFunction[~[String, ElExpr], (BinaryOper, ElExpr)] = {
	  case op~num => (BinaryOper(op), num)
	}
	private def arithSeq: PartialFunction[~[ElExpr, List[(BinaryOper, ElExpr)]], ElExpr] = {
		case num~Nil => num
		case num~rs if rs.size == 1 => BinaryExpr(num, rs(0)._1, rs(0)._2)
		case num~rs => MultiOpExpr(num, rs)
	}
	def numExpr1: Parser[ElExpr] = numExpr2~rep(("*" | "/")~numExpr2 ^^ opNumTuple) ^^ arithSeq
	def numExpr2: Parser[ElExpr] = rep(unaryOp)~value ^^ {
	  case Nil~value => value
		case ops~value => UnaryExpr(ops, value)
	}
  def unaryOp:Parser[UnaryOper] = (
	  "-" ^^ (x => NegativeOper) | 
		"!" ^^ (x => NotOper) | 
		"empty" ^^ (x => EmptyOper)
	)
  def value:Parser[ElExpr] = valuePrefix~rep(valueSuffix) ^^ {
		case pfx~Nil => pfx
	  case pfx~sfxes => Value(pfx, sfxes)
	}
  def valuePrefix:Parser[ElExpr] = literal | nonLiteralValuePrefix
  def literal:Parser[ElExpr] = (
	  booleanLiteral | 
		floatingPointNumber ^^ (x => NumberLiteral(x)) |
		/* remove prefix and suffix quote of string */
		stringLiteral ^^ (x => StringLiteral(x.substring(1, x.length - 1))) | 
		"null" ^^ (x => NullLiteral)
	)
  def booleanLiteral:Parser[ElExpr] = "true" ^^ (x => TrueLiteral) | "false" ^^ (x => FalseLiteral)
  
	def apply(text: String) = parseAll(elExpr, text) match {
    case Success(result, _) => result
    case NoSuccess(msg, _) => throw new ElParseException(msg)
  }
}
