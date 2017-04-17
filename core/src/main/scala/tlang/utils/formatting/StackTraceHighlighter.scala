package tlang.utils.formatting

import java.io.{PrintWriter, StringWriter}

import scala.util.parsing.combinator.RegexParsers

case class StackTraceHighlighter(formatting: Formatting) extends RegexParsers {

  import formatting._

  private val TextColor = Bold
  private val FileColor = Cyan + Bold
  private val IdColor   = Red + Bold
  private val MsgColor  = Yellow
  private val Indent    = "   "

  def apply(throwable: Throwable): String = apply(getStackTrace(throwable))
  def apply(stackTrace: String): String = {
    if (!formatting.useColor)
      return stackTrace

    parseAll(StackTraceParser(), stackTrace) match {
      case Success(res, _)      => res
      case NoSuccess(msg, next) => parseError(msg, next)
      case Failure(msg, next)   => parseError(msg, next)
    }
  }

  private def getStackTrace(throwable: Throwable): String = {
    val sw = new StringWriter()
    val pw = new PrintWriter(sw)
    throwable.printStackTrace(pw)
    sw.toString
  }

  private def parseError(msg: String, nxt: StackTraceHighlighter.this.Input) = {
    val s = nxt.source
    val offset = nxt.offset

    throw new RuntimeException(
      s"""
         |Failed to parse stack trace.
         |Message: $msg
         |
         |Parsed ($offset chars):
         |------------------------------------------------------------
         |${ s.subSequence(0, offset) }
         |------------------------------------------------------------
         |Remaining input:
         |------------------------------------------------------------
         |${ s.subSequence(offset, s.length) }
         |------------------------------------------------------------
     """.stripMargin.trim)
  }

  object StackTraceParser {

    def apply(): Parser[String] =
      description ~ stackTraceLine.+ ~ suppressed.? ~
      ("Caused by:" ~> description ~ stackTraceLine.+ ~ suppressed.?).* ^^ {
        case desc ~ stackPoses ~ more ~ rest =>
          formatStack(desc, stackPoses, more) +
          rest.map { case desc ~ stackPoses ~ more =>
            "\n" + TextColor("Caused by") + SymbolColor(":") + " " + formatStack(desc, stackPoses, more)
          }.mkString
      }

    // A description starts the stack trace eg.
    // "javax.servlet.ServletException: Something bad happened"
    private def description =
    clazz ~ (": " ~> descriptionMessage).? ^^ { case clazz ~ msg =>
      clazz + msg.map { msg => SymbolColor(":") + " " + MsgColor(msg) }.getOrElse("")
    }

    // A line in the stacktrace eg.
    // "at org.mortbay.jetty.Server.handle(Server.java:326)"
    private def stackTraceLine =
    "at" ~> clazz ~ source ^^ { case clazz ~ source =>
      TextColor("at") + " " + clazz + source
    }

    // Suppressed lines eg.
    // "... 27 more"
    private def suppressed =
    "..." ~> num <~ "more" ^^ { num =>
      SymbolColor("...") + " " + NumColor(num) + " " + TextColor("more")
    }

    // A class eg.
    // "org.mortbay.jetty.Server.handle"
    private def clazz =
    id ~ ("." ~> id).* ^^ { case id ~ rest =>
      IdColor(id) + rest.map { id => SymbolColor(".") + IdColor(id) }.mkString
    }

    // A source eg.
    // "(MyServlet.java:169)" or "(Unknown Source)"
    private def source =
    "(" ~ ("Unknown Source" | "Native Method" | file) ~ ")" ^^ { case _ ~ file ~ _ =>
      SymbolColor("(") + file + SymbolColor(")")
    }

    // A file name eg.
    // "MyServlet.java:169"
    private def file =
    (id <~ ".") ~ (id <~ ":") ~ num ^^ { case name ~ ending ~ num =>
      FileColor(name + "." + ending) + SymbolColor(":") + NumColor(num)
    }

    private val num                = """\d+""".r
    private val id                 ="""[0-9A-Za-z$_\-/]+""".r
    private val descriptionMessage = "(.|\n)+".r

    private def formatStack(desc: String, stackPoses: List[String], more: Option[String]) = {
      desc + "\n" +
      stackPoses.map(Indent + _).mkString("\n") +
      more.map(m => "\n" + Indent + m).getOrElse("")
    }
  }

}
