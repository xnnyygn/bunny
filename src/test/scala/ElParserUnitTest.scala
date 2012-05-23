import bunny.el._
import org.specs._

class ElParserUnitTest extends Specification {
  "ElParser" should {
    val instance = new JavaTokenElParser
    def parse(expr: String) = instance(expr)
	  "parse ${1} must return integer 1" in {
		  parse("${1}") must_== IntegerLiteral(1)
		}
		"parse ${1+2} must return binary expression integer 1 plus integer 2" in {
		  parse("${1+2}") must_== BinaryExpr(IntegerLiteral(1), PlusOper, IntegerLiteral(2))
		}
		"parse ${1+2-1.0} must return multiple operation expression" in {
		  parse("${1+2-1.0}") must_== MultiOpExpr(IntegerLiteral(1), List(
			  (PlusOper, IntegerLiteral(2)), 
				(MinusOper, FloatLiteral(1.0))
			))
		}
		"parse ${1*2} must return binary expression integer 1 multiply integer 2" in {
		  parse("${1*2}") must_== BinaryExpr(IntegerLiteral(1), MultiplyOper, IntegerLiteral(2))
		}
		"parse ${1>2} must return binary expression integer 1 greater than integer 2" in {
		  parse("${1>2}") must_== BinaryExpr(IntegerLiteral(1), GreaterThanOper, IntegerLiteral(2))
		}
		"parse ${1<2 && 2>3} must return binary expression" in {
		  parse("${1<2 && 2>3}") must_== BinaryExpr(
			  BinaryExpr(IntegerLiteral(1), LessThanOper, IntegerLiteral(2)),
				AndOper,
				BinaryExpr(IntegerLiteral(2), GreaterThanOper, IntegerLiteral(3))
			)
		}
		"parse ${1!=2 || 3==4} must return binary expression" in {
		  parse("${1!=2 || 3==4}") must_== BinaryExpr(
			  BinaryExpr(IntegerLiteral(1), NotEqualOper, IntegerLiteral(2)),
				OrOper,
				BinaryExpr(IntegerLiteral(3), EqualOper, IntegerLiteral(4))
			)
		}
		"parse ${empty a.b} must return unary expression" in {
		  parse("${empty a.b}") must_== UnaryExpr(List(EmptyOper), Value(Identifier("a"), List(PropertyName(Identifier("b")))))
		}
	}
}
