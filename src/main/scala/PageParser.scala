package bunny.page

import java.io.{InputStream, ByteArrayInputStream}
import javax.xml.parsers.SAXParserFactory
import org.xml.sax.helpers.DefaultHandler
import org.xml.sax.{InputSource, Attributes}

trait PageParser {
	def apply(text: String, encoding: String): List[Token] = {
	  if(text.isEmpty) List.empty[Token]
	  else apply(new ByteArrayInputStream(text.getBytes(encoding)), encoding)
	}
  def apply(input: InputStream, encoding: String): List[Token]
}

class SaxPageParser extends PageParser {
  class StringTokenBuilder {
	  private var b = new StringBuilder 
	  def appendChars(chars: Array[Char], start: Int, length: Int): Unit = b.appendAll(chars, start, length)
		def appendTagStart(name: String, attrs: List[(String, String)]): Unit = b.append('<').append(name).append(attrs.map{kv =>
		  " " + kv._1 + "=\"" + kv._2 + "\""
		}.mkString).append('>')
		def appendTagEnd(name: String): Unit = b.append("</").append(name).append('>')
		def isEmpty = b.length == 0
		def flush = {
		  val t = new StringToken(b.toString)
			b = new StringBuilder
			t
		}
	}
  class PageHandler extends DefaultHandler {
		private var seq: TokenSeq = new ListBufferTokenSeq
		private val builder = new StringTokenBuilder
	  def tokens: List[Token] = seq.toDetachedOnes
		override def startElement(uri: String, name: String, qName: String, attrs: Attributes): Unit = {
	    if(isServerTag(qName)){
			  if(!builder.isEmpty) seq.append(builder.flush)
				val t = new ParsingServerTagToken(qName, attrsToPairs(attrs).toMap, seq)
				seq.append(t)
				seq = t
			}else builder.appendTagStart(qName, attrsToPairs(attrs))
		}
		private def attrsToPairs(attrs: Attributes): List[(String, String)] = {
		  (for(i <- 0 until attrs.getLength) yield (attrs.getQName(i), attrs.getValue(i))).toList
		}
		private final val serverTagRegex = """\w+:\w+""".r
		private def isServerTag(name: String): Boolean = serverTagRegex.findFirstIn(name) != None
		override def characters(chars: Array[Char], start: Int, length: Int): Unit = {
		  builder.appendChars(chars, start, length)
		}
		override def endElement(uri: String, name: String, qName: String): Unit = {
		  if(isServerTag(qName)) {
				if(!builder.isEmpty) seq.append(builder.flush)
			  seq = seq.getParent
			} else builder.appendTagEnd(qName)
		}
		override def endDocument: Unit = {
		  seq.append(builder.flush)
		}
	}
  def apply(input: InputStream, encoding: String): List[Token] = {
	  val h = new PageHandler
    val src = new InputSource(input)
    src.setEncoding(encoding)
    saxOne.parse(src, h) 
		h.tokens
	}
	private def saxOne = {
	  val f = SAXParserFactory.newInstance
		f.setNamespaceAware(false)
		f.newSAXParser
	}
}
