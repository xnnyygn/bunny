package bunny.page

sealed trait Token 

sealed trait TokenSeq {
  def append(t: Token): Unit
	def getParent: TokenSeq
	def toList: List[Token]
	def toDetachedOnes: List[Token] = toDetachedOnes(toList)
	private def toDetachedOnes(tokens: List[Token]): List[Token] = tokens.map{t => 
		if(!t.isInstanceOf[ParsingServerTagToken]) t
    else {
		  val p = t.asInstanceOf[ParsingServerTagToken]
			new ServerTagToken(p.name, p.attrs, toDetachedOnes(p.children))
		}
	}
}

case class StringToken(text: String) extends Token {
  override def toString = {
		def abbr(src: String, max: Int) = {
		  val length = src.length
			if(length < max) src
		  else src.substring(0, scala.math.min(length, max - 3)) + "..."
		}
		def removeWhitespace(src: String) = src.replaceAll("\\s", "")
	  "StringToken[%s]".format(abbr(removeWhitespace(text), 10))
	}
}

sealed abstract class AbstractTokenSeq extends TokenSeq {
  protected val buf = scala.collection.mutable.ListBuffer.empty[Token]
	def append(t: Token) = buf += t
	def toList = buf.toList
}

class ListBufferTokenSeq extends AbstractTokenSeq {
	def getParent = throw new UnsupportedOperationException("no parent of list buffer token sequence")
}

sealed class ParsingServerTagToken(val name: String, val attrs: Map[String, String], parent: TokenSeq) extends AbstractTokenSeq with Token {
	def getParent = parent
	def children = toList
	override def toString = "ParsingServerTagToken[name=%s, attrs=%s, children=%s]".format(name, attrs, children.mkString("[", ",", "]"))
}

case class ServerTagToken(name: String, attrs: Map[String, String], children: List[Token]) extends Token
