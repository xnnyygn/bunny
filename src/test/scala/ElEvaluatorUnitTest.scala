import bunny.el._
import org.specs._

class ElEvaluatorUnitTest extends Specification {
  "ElEvaluator" should {
    val instance = new DefaultElEvaluator(new JavaTokenElParser)
    def eval(expr: String, ctx: Map[String, Any] = Map()) = instance(expr, ctx)
	  "eval ${} must throw exception" in {
		  eval("${}") must throwA[ElParseException]
		}
		"eval ${1} must return 1" in {
		  eval("${1}") must_== 1
		}
		"eval ${1 + 2} must return 3" in {
		  eval("${1 + 2}") must_== 3
		}
		"eval ${1 + 2 * 3} must return 7" in {
		  eval("${1 + 2 * 3}") must_== 7
		}
		"eval ${1 < 2 && 2 < 4} must return true" in {
		  eval("${1 < 2 && 2 < 4}") must_== true
		}
		"eval ${a} and a = 'foo' must return 'foo'" in {
		  eval("${a}", Map("a" -> "foo")) must_== "foo"
		}
		"eval ${a + 1} and a = 2 must return 3" in {
		  eval("${a + 1}", Map("a" -> 2)) must_== 3
		}
		"eval ${sqrt(4) + 1} must return 3" in {
		  eval("${sqrt(4) + 1}") must_== 3
		}
		"eval ${true} must return true" in {
		  eval("${true}") must_== true
		}
		"eval ${true && false} must return false" in {
		  eval("${true && false}") must_== false
		}
		"eval ${!(a>2)} and a = 3 must return false" in {
		  eval("${!(a>2)}", Map("a" -> 3)) must_== false
		}
		class Foo(val bar: Int)
		"eval ${foo.bar + 1} and foo = Foo(bar = 2) must return 3" in {
		  eval("${foo.bar + 1}", Map("foo" -> new Foo(bar = 2))) must_== 3
		}
		"eval ${\"foo\"} must return string foo" in {
		  eval("${\"foo\"}") must_== "foo"
		}
		"eval ${a[1]} and a = [1, 2] must return 2" in {
		  eval("${a[1]}", Map("a" -> Array(1, 2))) must_== 2
		}
		"eval ${a[0].bar + 4} and a = List(Foo(bar = 3)) must return 7" in {
		  eval("${a[0].bar + 4}", Map("a" -> List(new Foo(bar = 3)))) must_== 7
		}
		"eval ${m[\"a\"]} and m = Map(\"a\" -> 100) must return 100" in {
		  eval("${m[\"a\"]}", Map("m" -> Map("a" -> 100))) must_== 100
		}
		"eval ${list.size} and list = List(1, 2) must return 2" in {
		  eval("${list.size}", Map("list" -> List(1, 2))) must_== 2
		}
	}
}
