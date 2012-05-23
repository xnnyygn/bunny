package bunny.tag

import bunny.page.RenderContext

trait TagResolver {
  def findOrExecute(name: String, ctx: RenderContext, attrs: TagAttrs, body: TagBody): String
}

class NoSuchTagException(msg: String) extends TagLibException(msg)

class SimpleTagResolver(tags: Map[String, TagLib]) extends TagResolver {
  def findOrExecute(name: String, ctx: RenderContext, attrs: TagAttrs, body: TagBody) = {
	  tags.get(name).map(_.apply(ctx, attrs, body)) match {
		  case Some(result) => result
			case _ => throw new NoSuchTagException("no such tag [" + name + "]")
		}
	}
}

class MethodReflectTagResolver(targets: List[(String, Any)]) extends SimpleTagResolver(
	targets.map{t =>
	  val target = t._2
	  target.getClass.getMethods.filter{m =>
				m.getParameterTypes == Array(
          classOf[RenderContext], 
          classOf[TagAttrs], 
          classOf[TagBody]) &&
				m.getReturnType == classOf[String]
			}.map{m =>
		  (t._1 + ":" + m.getName, new MethodReflectTagLib(m, target))
		}.toList
	}.flatten.toMap)

