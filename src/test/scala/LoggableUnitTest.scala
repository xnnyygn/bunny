import org.specs._
import bunny.logging._

class LoggableUnitTest extends Specification {
  "Loggable" should {
		"info 'foo' must log '[INFO] foo'" in {
			val out = new SimpleOutputer
			val loggable = new Loggable {
				override lazy val outputer = out
			}
			loggable.info("foo")
			out.get must_== "[INFO] foo\n"
		}
  }
}
