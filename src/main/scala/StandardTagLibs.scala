package bunny.tag

import bunny.page.RenderContext

object StandardTagLibs {
  val out = new TagLib {
	  private final val KEY_VALUE = "value"
	  def apply(ctx: RenderContext, attrs: TagAttrs, body: TagBody): String = {
			scala.xml.Utility.escape(attrs.getOrEx[Any](KEY_VALUE).toString)
		}
	}
  class UnsupportedCollectionException(msg: String) extends TagLibException(msg)
	val forEach = new TagLib {
		private final val KEY_ITEMS = "items"
		private final val KEY_VAR = "var"
		private final val DEFAULT_VAR_NAME = "it"
	  def apply(ctx: RenderContext, attrs: TagAttrs, body: TagBody): String = {
		  def asIterable(obj: Any): List[Any] = obj match {
			  case l: List[_] => l
				case _ => throw new UnsupportedCollectionException("cannot iterable on [" + obj + "]")
			}
			val varName = attrs.getOrElse[String](KEY_VAR, DEFAULT_VAR_NAME)
		  val result = asIterable(attrs.getOrEx(KEY_ITEMS)).map{x =>
			  ctx.update(varName, x)
				body(ctx)
			}.mkString
			ctx.remove(varName)
			result
		}
	}
  class UnsupportedDateException(msg: String) extends TagLibException(msg)
  val formatDate = new TagLib {
    private final val KEY_FORMAT = "format"
    private final val KEY_DATE = "date"
    def apply(ctx: RenderContext, attrs: TagAttrs, body: TagBody): String = {
      import java.util.{Calendar, Date}, java.text.SimpleDateFormat
      def getDateInMillis: Long = attrs.getOrElse[Any](KEY_DATE, Calendar.getInstance) match {
        case l: Long => l
        case d: Date => d.getTime
        case c: Calendar => c.getTimeInMillis
        case x => throw new UnsupportedDateException("unexpected date type [" + x + "]")
      }
      new SimpleDateFormat(attrs.getOrEx[String](KEY_FORMAT)).format(getDateInMillis)
    }
  }
  val ifOne = new TagLib {
    private final val KEY_TEST = "test"
    final val KEY_LAST_TEST = "lastTest"
    def apply(ctx: RenderContext, attrs: TagAttrs, body: TagBody) = {
      val lastTest = attrs.getOrEx[Boolean]("test")
      ctx.update(KEY_LAST_TEST, lastTest)
      if(lastTest) body(ctx) else ""
    }
  }
  class IllegalElseException(msg: String) extends TagLibException(msg)
  val elseOne = new TagLib {
    def apply(ctx: RenderContext, attrs: TagAttrs, body: TagBody) = {
      val key = ifOne.KEY_LAST_TEST
      def lastTest: Boolean = ctx.get[Boolean](key) match {
        case Some(value) => ctx.remove(key); value
        case _ => throw new IllegalElseException("else must follow if or else if")
      }
      if(!lastTest) body(ctx) else ""
    }
  }
}
