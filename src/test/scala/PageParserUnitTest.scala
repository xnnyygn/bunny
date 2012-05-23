import bunny.page._
import org.specs._

class PageParserUnitTest extends Specification {
  "PageParser" should {
    val instance = new SaxPageParser
    def parse(text: String) = instance(text, "UTF-8")
		"parse empty text must return no token" in {
		  parse("").isEmpty must beTrue
		}
		"simple page must have 3 tokens" in {
		  val tokens = parse("""
			  <!DOCTYPE HTML>
			  <html lang="en">
			  <head>
			    <meta charset="UTF-8" />
			    <title>Blog List</title>
			  </head>
			  <body>
				  <h1>Blog List</h1>
					<s:foreach items="${blogs}" var="b">
					  <div class="blog">
						  <h2><s:out value="${blog.title}" /></h2>
							<p><s:out value="${blog.content}" /></p>
							<s:foreach items="${blog.comments}" var="c">
							  <div class="comment">
								  <p><c:out value="${c.content}" /></p>
								</div>
							</s:foreach>
						</div>
					</s:foreach>
			  </body>
			  </html>
			""")
			tokens.length must_== 3
		}
	}
}
