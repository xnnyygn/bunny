import bunny.page._
import bunny.tag.{SimpleTagResolver, StandardTagLibs => STL}
import bunny.el.{DefaultElEvaluator, JavaTokenElParser}
import org.specs._

object PageRendererUnitTest extends Specification {
  "PageRenderer" should {
    val renderer = new DefaultPageRenderer(new SimpleTagResolver(Map(
      "b:out" -> STL.out,
      "b:forEach" -> STL.forEach,
      "b:if" -> STL.ifOne
    )), new DefaultElEvaluator(new JavaTokenElParser()))
    def renderOne(token: Token, ctx: RenderContext = new MapRenderContext): String = renderer(List(token), ctx)
	  "render string token 'Hello, world!' must be 'Hello, world!'" in {
		  renderOne(StringToken("Hello, world!")) must_== "Hello, world!"
		}
		"render server tag token b:out value '<xml>' must be '&lt;xml&gt;'" in {
		  renderOne(ServerTagToken("b:out", Map("value" -> "<xml>"), List.empty[Token])) must_== "&lt;xml&gt;"
		}
    "render server tag token b:forEach items 1 to 10 must be execute body ten times" in {
      renderOne(
        ServerTagToken("b:forEach", Map("items" -> "${numbers}"), List(StringToken("Hi!"))),
        new MapRenderContext(Map("numbers" -> (1 to 10).toList))
      ) must_== "Hi!" * 10
    }
    "render server tag token b:forEach items List(a, b, c) and output it must return abc" in {
      renderOne(
        ServerTagToken("b:forEach", Map("items" -> "${letters}"), List(
          ServerTagToken("b:out", Map("value" -> "${it}"), List())
        )),
        new MapRenderContext(Map("letters" -> "abc".toList))
      ) must_== "abc"
    }
    "render server tag token b:forEach items List(1, 2) and inner forEach List(a, b) and output 1a1b2a2b" in {
      renderOne(
        ServerTagToken("b:forEach", Map("items" -> "${numbers}", "var" -> "x"), List(
          ServerTagToken("b:forEach", Map("items" -> "${letters}", "var" -> "y"), List(
            ServerTagToken("b:out", Map("value" -> "${x}"), List()),
            ServerTagToken("b:out", Map("value" -> "${y}"), List())
          )
        )
      )), new MapRenderContext(Map("numbers" -> List(1, 2), "letters" -> List("a", "b")))) must_== "1a1b2a2b"
    }
	}
}
