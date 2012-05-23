package bunny.logging

trait Loggable {
  def debug(message: => String): Unit = log("DEBUG", message)
	def info(message: => String): Unit = log("INFO", message)
	
	protected lazy val outputer: Outputer = ConsoleOutputer
	private def log(level: String, message: => String): Unit = {
	  outputer.writeln("[" + level + "] " + message)
	}
}

trait Outputer {
  def writeln(line: => String): Unit
}

class SimpleOutputer extends Outputer {
  val builder = new StringBuilder
	def writeln(line: => String) = builder.append(line).append('\n')
	def get = builder.toString
}

object ConsoleOutputer extends Outputer {
  def writeln(line: => String) = println(line)
}

object NullOutputer extends Outputer {
  def writeln(line: => String) = ()
}
