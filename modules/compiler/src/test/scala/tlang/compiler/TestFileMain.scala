package tlang
package compiler

import tlang.compiler.Main.CompilerFlags
import tlang.compiler.argument._
import tlang.compiler.execution.Compiler
import tlang.compiler.imports.ClassPath
import tlang.compiler.messages.{CompilerMessages, DefaultReporter}
import tlang.compiler.output.help.HelpOutput
import tlang.compiler.output.{JSONOutputHandler, MessageOutput, PrettyOutputHandler}
import tlang.formatting.{ErrorStringContext, Formatter}
import tlang.options.argument._
import tlang.options.{FlagArgument, Options}
import tlang.utils.{FileSource, Logging}

object TestFileMain extends Logging {
  case object TestHelpFlag extends HelpFlag(TestFlags)

  val TestFlags: Set[FlagArgument[_]] = Set(
    AsciiFlag,
    ClassPathFlag,
    ColorSchemeFlag,
    JSONFlag,
    LineWidthFlag,
    LogLevelFlag,
    MessageContextFlag,
    NoColorFlag,
    PrintOutputFlag,
    TabWidthFlag,
    VerboseFlag,
    TestHelpFlag
  )

  def main(args: Array[String]): Unit = {
    val options = parseOptions(args)
    implicit val formatter: Formatter = Formatter(
      lineWidth = options(LineWidthFlag),
      tabWidth = options(TabWidthFlag),
      colorScheme = options(ColorSchemeFlag),
      useColor = !(options(NoColorFlag) || options(JSONFlag)),
      asciiOnly = options(AsciiFlag)
    )

    Logging.DefaultLogSettings.formatter = formatter
    Logging.DefaultLogSettings.logLevel = options(LogLevelFlag)

    val ctx = createContext(options)

    val helpArgs = options(CompilerHelpFlag)

    if (options.isEmpty || (HelpFlag.defaultArg in helpArgs)) {
      ctx.output += HelpOutput(Constants.TesterCommandName, TestFlags)
      sys.exit(1)
    }

    import ctx.formatter._

    val sources = options(TFilesArgument).map(FileSource(_))
    val success = sources forall { source =>
      val fileTester = CompilerFileTester(source.file, ctx, Compiler.FrontEnd)
      val res = fileTester.execute()
      if (res.success) {
        ctx.output += MessageOutput(s"${ Green("Test of file") } ${ source.getDescription(Green + Bold) } ${ Green("was successful.") }")
      } else {
        ctx.output += MessageOutput(s"${ Red("Test of file") } ${ source.getDescription(Red + Bold) } ${ Red("failed.") }")
        ctx.output += MessageOutput(res.message)
      }
      res.success
    }

    sys.exit(if (success) 0 else 1)
  }

  private def parseOptions(args: Array[String]): Options = {
    implicit val errorContext: ErrorStringContext = ErrorStringContext()(Formatter.SimpleFormatter)
    try {
      Options(flags = CompilerFlags, positionalArgument = Some(TFilesArgument), arguments = args)
    } catch {
      case e: IllegalArgumentException =>
        println(e.getMessage)
        sys.exit(1)
    }
  }

  private def createContext(options: Options)(implicit formatter: Formatter): Context = {
    info"Creating context with options $options"

    val messages = CompilerMessages(maxErrors = -1)
    val outputHandler = if (options(JSONFlag)) JSONOutputHandler() else PrettyOutputHandler()
    Context(
      reporter = DefaultReporter(messages = messages),
      outputHandler = outputHandler,
      classPath = ClassPath.Default ++ options(ClassPathFlag),
      options = options
    )
  }
}
