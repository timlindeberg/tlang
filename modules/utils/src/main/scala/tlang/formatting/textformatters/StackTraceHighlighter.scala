package tlang.formatting.textformatters

import tlang.formatting.Formatting
import tlang.utils.Extensions._

import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

case class StackTraceHighlighter(formatting: Formatting, failOnError: Boolean = false) {

  def apply(throwable: Throwable): String = apply(throwable.stackTrace)
  def apply(stackTrace: String): String = {
    if (!formatting.useColor)
      return stackTrace


    if (stackTrace.trim.isEmpty) "" else StackTraceParser.parse(stackTrace)
  }


  object StackTraceParser extends RegexParsers {


    import formatting._

    private val TextColor   = Bold
    private val FileColor   = Cyan + Bold + Underline
    private val IdColor     = Red + Bold
    private val MethodColor = Blue + Bold
    private val MsgColor    = Yellow
    private val Indent      = "   "

    // Override whitespaces so that spaces and tabs are skipped but not new lines.
    override val whiteSpace: Regex = "".r

    def parse(stackTrace: String): String = {
      parseAll(parser(), stackTrace) match {
        case Success(res, _)      => res
        case NoSuccess(msg, next) =>
          if (failOnError) sys.error(parseError(msg, next)) else stackTrace
      }
    }


    def parser(): Parser[String] =
      exception ~ ("Caused by:" ~> exception).* ^^ { case exc ~ more =>
        if (more.isEmpty)
          exc
        else
          exc + NL + more.map(exc => TextColor("Caused by:") + exc).mkString(NL)
      }

    private def exception = {
      description ~ stackTraceLine.+ ~ suppressed.? ^^ { case desc ~ stackLines ~ more =>
        desc +
          stackLines.mkString(NL) +
          more.map(NL + Indent + _).getOrElse("")
      }
    }


    // A description starts the stack trace eg.
    // "javax.servlet.ServletException: Something bad happened"
    private def description = {
      exceptionInThread.? ~ clazz ~ ((":" ~> descriptionMessage) | newLine) ^^ { case exception ~ clazz ~ msg =>
        exception.getOrElse("") + clazz + msg
      }
    }


    // The description of the message. Multiple lines of any character not starting with at
    private def descriptionMessage = {
      (not("\tat") ~> ".*".r <~ newLine).+ ^^ { lines =>
        val desc = lines.mkString(NL)
        SymbolColor(":") + MsgColor(desc) + NL
      }
    }


    // Specifies which thread the exception occurred in eg.
    // 'Exception in thread "main"'
    private def exceptionInThread = {
      "Exception in thread \"" ~> id <~ "\"" ^^ { threadName =>
        TextColor("Exception in thread " + Blue('"' + threadName + '"'))
      }
    }


    // A line in the stacktrace eg.
    // "at org.mortbay.jetty.Server.handle(Server.java:326)"
    private def stackTraceLine = {
      "\tat" ~> methodCall ~ source <~ newLine ^^ { case clazz ~ source =>
        Indent + TextColor("at") + clazz + source
      }
    }


    // Suppressed lines eg.
    // "... 27 more"
    // "... 365 common frames omitted"
    private def suppressed = {
      "\t... " ~> num <~ ((" more" | " common frames omitted") ~ newLine) ^^ { num =>
        SymbolColor("...") + NumColor(num) + TextColor(" more")
      }
    }


    // A clazz eg.
    // "org.mortbay.jetty.Server"
    private def clazz = {
      id ~ ("." ~> id).* ^^ { case id ~ rest =>
        IdColor(id) + rest.map { id => SymbolColor(".") + IdColor(id) }.mkString
      }
    }


    // A method call eg.
    // "org.mortbay.jetty.Server.handle"
    private def methodCall = {
      id ~ ("." ~> id).* ^^ { case id ~ rest =>
        if (rest.isEmpty) {
          IdColor(id)
        } else {
          val clazz = rest.dropRight(1).map { id => SymbolColor(".") + IdColor(id) }.mkString
          val method = SymbolColor(".") + MethodColor(rest.last)
          IdColor(id) + clazz + method
        }
      }
    }


    // A source eg.
    // "(Unknown Source)"
    // "(MyServlet.java:169)" or
    // "(MyClass.java:431) [struts-1.2.9.jar:1.2.9]"
    private def source =
      ("(" ~> ("Unknown Source" | "Native Method" | file) <~ ")") ~ jar.? ^^ { case file ~ jar =>
        SymbolColor("(") + FileColor(file) + SymbolColor(")") + (if (jar.isDefined) " " + jar.get else "")
      }


    // A file name eg.
    // "MyServlet.java:169"
    // "Main.scala"
    private def file = {
      """[0-9A-Za-z$_\-/:\. ]+""".r ~ (":" ~> num).? ^^ { case name ~ num =>
        FileColor(name) + num.map { SymbolColor(":") + NumColor(_) }.getOrElse("")
      }
    }


    // A jar file eg.
    // "[struts-1.2.9.jar:1.2.9]"
    // These don't actually occur in stack traces but are produced by Logback.
    private def jar = {
      (" " ~ ("~".? ~ "[")) ~> """[0-9A-Za-z$_\-/:\.]+""".r <~ "]" ^^ { id =>
        SymbolColor("[") + MethodColor(id) + SymbolColor("]")
      }
    }


    private val num     = """\d+""".r
    private val id      = """[0-9A-Za-z$_\-/<> ]+""".r
    private val newLine = "\r?\n".r


    private def parseError(msg: String, nxt: Input): String = {
      val s = nxt.source
      val offset = nxt.offset
      s"""|Failed to parse stack trace.
          |Message:
          |--------------------------------------------------------------------------------
          |${ msg.replaceAll("\r", "\\\\r").replaceAll("\n", "\\\\n") }
          |--------------------------------------------------------------------------------
          |
          |Parsed ($offset / ${ s.length } chars):
          |--------------------------------------------------------------------------------
          |>>>>>${ s.subSequence(0, offset) }<<<<<${ s.subSequence(offset, s.length) }
          |--------------------------------------------------------------------------------
          |""".stripMargin.trim
    }

  }

}
