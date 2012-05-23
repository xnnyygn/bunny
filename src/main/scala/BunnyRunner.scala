package bunny

import bunny.page.{DefaultPageRenderer, SaxPageParser, MapRenderContext}
import bunny.el.{DefaultElEvaluator, JavaTokenElParser}
import bunny.tag.{SimpleTagResolver, StandardTagLibs => STL}
import java.io.FileInputStream
import java.util.Date

object BunnyRunner {
  case class Blog(title: String, content: String, gmtCreate: Date = new Date)
  def main(args: Array[String]): Unit = {
    println(render("src/main/resources/blog-list.ssp", Map(
      "blogs" -> List(
        Blog("title0", "content0"),
        Blog("title1", "content1")
      ), "total" -> 2
    )))
  }
  private def render(path: String, ctx: Map[String, Any]): String = {
    val renderer = new DefaultPageRenderer(
      new SimpleTagResolver(Map(
        "b:out" -> STL.out,
        "b:forEach" -> STL.forEach,
        "b:formatDate" -> STL.formatDate,
        "b:if" -> STL.ifOne,
        "b:elseIf" -> STL.ifOne,
        "b:else" -> STL.elseOne
      )),
      new DefaultElEvaluator(new JavaTokenElParser))
    val parser = new SaxPageParser
    renderer(parser(new FileInputStream(path), "UTF-8"), new MapRenderContext(ctx))
  }
}
