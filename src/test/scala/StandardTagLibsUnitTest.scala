import bunny.tag._
import bunny.page.{RenderContext, MapRenderContext}
import StandardTagLibs._
import org.specs._

object StandardTagLibsUnitTest extends Specification {
  "StandardTagLibs" should {
	  "out should output the value" in {
      out(new MapRenderContext(), new MapTagAttrs(Map("value" -> "foo")), MockTagBody) must_== "foo"
		}
    "forEach 1 until 10 should output 1 until 10" in {
      forEach(new MapRenderContext(), new MapTagAttrs(Map("items" -> (1 until 10).toList)), new TagBody {
        def apply(ctx: RenderContext) = ctx.getOrElse[Any]("it", "").toString
      }) must_== "123456789"
    }
    "formatDate 2012-05-22 13:43:57 with format yyyy-MM-dd HH:mm:ss must be 2012-05-22 13:43:57" in {
      import java.util.{GregorianCalendar => GCalendar}
      formatDate(new MapRenderContext(), new MapTagAttrs(Map(
        "format" -> "yyyy-MM-dd HH:mm:ss", 
        "date" -> new GCalendar(2012, 5 - 1, 22, 13, 43, 57)
      )), MockTagBody) must_== "2012-05-22 13:43:57"
    }
    "if test = true should execute body" in {
      ifOne(new MapRenderContext(), new MapTagAttrs(Map("test" -> true)), new TagBody {
        def apply(ctx: RenderContext) = "in if"
      }) must_== "in if"
    }
    "else if lastTest = false in context will execute body" in {
      elseOne(new MapRenderContext(Map(ifOne.KEY_LAST_TEST -> false)), new MapTagAttrs, new TagBody {
        def apply(ctx: RenderContext) = "in else"
      }) must_== "in else"
    }
	}
}
