package bunny.page

import bunny.BunnyException
import bunny.tag.{TagBody, TagResolver, MapTagAttrs}
import bunny.el.ElEvaluator

trait PageRenderer {
  def apply(tokens: List[Token], ctx: RenderContext): String
}

trait RenderContext {
  def get[T](key: String): Option[T]
	def getOrEx[T](key: String): T = get(key) match {
	  case Some(value) => value
		case _ => throw new ValueNotFoundException("[" + key + "] not found")
	}
	def getOrElse[T](key: String, default: T): T = get(key).getOrElse(default)
	def update(key: String, value: Any): RenderContext
	def remove(key: String): RenderContext
  def toMap: Map[String, Any]
}

class ValueNotFoundException(msg: String) extends BunnyException(msg)

class MapRenderContext(init: Map[String, Any] = Map()) extends RenderContext {
  private final val map = scala.collection.mutable.Map(init.toSeq: _*)
	def get[T](key: String) = map.get(key).map(_.asInstanceOf[T])
	def update(key: String, value: Any) = {map.update(key, value); this}
	def remove(key: String) = {map -= key; this}
  def toMap = map.toMap
}

class UnsupportedTokenException(msg: String) extends BunnyException(msg)

class RenderableTagBody(tokens: List[Token], renderer: PageRenderer) extends TagBody {
  def apply(ctx: RenderContext) = renderer(tokens, ctx)
}

class DefaultPageRenderer(tagRlr: TagResolver, el: ElEvaluator) extends PageRenderer {
  private final val elPattern = """\$\{.+}""".r
  def apply(tokens: List[Token], ctx: RenderContext): String =  {
    def evalValue(value: String) = elPattern.findFirstIn(value) match {
      case Some(_) => el(value, ctx.toMap)
      case _ => value
    }
    def evalAttrs(attrs: Map[String, String]) = attrs.map{t => (t._1, evalValue(t._2))}
    tokens.map{t => t match {
      case StringToken(text) => text
      case ServerTagToken(name, attrs, children) => 
        tagRlr.findOrExecute(name, ctx, 
          new MapTagAttrs(evalAttrs(attrs)), new RenderableTagBody(children, this))
      case _ => throw new UnsupportedTokenException("unknown token [" + t + "]")
    }}.mkString
  }
}
