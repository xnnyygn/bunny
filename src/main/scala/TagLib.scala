package bunny.tag

import bunny.page.RenderContext
import bunny.BunnyException

trait TagLib {
  def apply(ctx: RenderContext, attrs: TagAttrs, body: TagBody): String
}

trait TagAttrs {
  def get[T](key: String): Option[T]
	def getOrEx[T](key: String): T = get(key) match {
	  case Some(value) => value
		case _ => throw new AttrNotFoundException("[" + key + "] is required")
	}
	def getOrElse[T](key: String, default: T): T = get(key).getOrElse(default)
}

class AttrNotFoundException(msg: String) extends TagLibException(msg)

class MapTagAttrs(map: Map[String, Any] = Map()) extends TagAttrs {
	def get[T](key: String) = map.get(key).map(_.asInstanceOf[T])
}

trait TagBody {
  def apply(ctx: RenderContext): String
}

object MockTagBody extends TagBody {
  def apply(ctx: RenderContext) = ""
}

abstract class TagLibException(msg: String) extends BunnyException(msg)

class MethodReflectTagLib(method: java.lang.reflect.Method, target: Any) extends TagLib {
  def apply(ctx: RenderContext, attrs: TagAttrs, body: TagBody): String = {
		method.invoke(target, ctx, attrs, body).asInstanceOf[String]
	}
}

