package tlang.repl

import java.io.File
import java.nio.file.Files

import tlang.compiler.Context
import tlang.compiler.ast.PrettyPrinter
import tlang.compiler.error.{DefaultReporter, Formatting}
import tlang.compiler.options.Flags._
import tlang.compiler.options.Options

/**
  * Created by Tim Lindeberg on 2/13/2017.
  */
object Main {

  val VersionNumber = "0.0.1"


  def main(args: Array[String]): Unit = {
    val options = Options(args)
    tlang.compiler.Main.checkTHome()

    if (options(Version)) {
      printVersion()
      sys.exit()
    }

    if (options(Help).nonEmpty) {
      printHelp(options.formatting, options(Help))
      sys.exit()
    }


    val tempDir = Files.createTempDirectory("repl").toFile
    val context = createContext(options, tempDir)
    val replLoop = ReplLoop(context)

    replLoop.start()

    tempDir.delete()
  }

  private def printVersion(): Unit = println(s"T-Repl $VersionNumber")

  private def createContext(options: Options, tempDir: File): Context = {
    val formatting = options.formatting
    Context(
      reporter = DefaultReporter(
        suppressWarnings = options(SuppressWarnings),
        warningIsError = options(WarningIsError),
        formatting = formatting,
        maxErrors = 25,
        errorContext = 0
      ),
      outDirs = Set(tempDir),
      formatting = formatting,
      printer = PrettyPrinter(formatting.colors)
    )
  }


  private def printHelp(formatting: Formatting, args: Set[String] = Set("")) = {
    args foreach { arg =>
      import formatting._
      import formatting.colors._

      val help = Flag.get(arg) match {
        case Some(flag) =>
          val header = flag.flagName(formatting)
          makeBox(header, List(flag.extendedDescription(formatting)))
        case None       =>
          val tcomp = Green("tcomp")
          val options = Blue("options")
          val source = Blue("source files")
          val header = s"> $tcomp <$options> <$source> \n\n" + Bold(Magenta("Options"))

          val flags = Flag.All.map { flag => (flag.flagName(formatting), flag.description(formatting)) }
          makeBoxWithColumn(header, flags)
      }
      print(help)
    }
  }

}
