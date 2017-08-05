package tlang.compiler

import tlang.Constants._
import tlang.Context
import tlang.compiler.analyzer.{Flowing, Naming, Typing}
import tlang.compiler.ast.Parsing
import tlang.compiler.ast.Trees._
import tlang.compiler.code.{CodeGeneration, Lowering}
import tlang.compiler.error._
import tlang.compiler.imports.ClassPath
import tlang.compiler.lexer.Lexing
import tlang.compiler.modification.Templating
import tlang.compiler.options.{Flags, Options}
import tlang.utils.Extensions._
import tlang.utils.formatting.Formatting
import tlang.utils.{FileSource, ProgramExecutor, Source}

object Main extends MainErrors {

  import Flags._

  val FrontEnd: CompilerPhase[Source, CompilationUnit] =
    Lexing andThen Parsing andThen Templating andThen
      Naming andThen Typing andThen Flowing andThen Lowering


  val CompilerPhases = List(
    Lexing,
    Parsing,
    Templating,
    Naming,
    Typing,
    Flowing,
    Lowering,
    CodeGeneration
  )

  def main(args: Array[String]) {
    val options = Options(args)

    if (args.isEmpty) {
      print(helpInfo(options.formatting))
      sys.exit(1)
    }

    printHelp(options)

    if (!isValidTHomeDirectory(TDirectory))
      FatalInvalidTHomeDirectory(TDirectory, THome)

    if (options.files.isEmpty)
      FatalNoFilesGiven()

    val ctx = createContext(options)

    if (options(Verbose)) printFilesToCompile(ctx)

    val CUs = runFrontend(ctx)

    CodeGeneration.execute(ctx)(CUs)

    if (ctx.reporter.hasWarnings)
      print(ctx.reporter.messages.formattedWarnings)

    if (options(Verbose))
      printExecutionTimes(ctx)

    if (options(Exec))
      executeProgram(ctx, CUs)
  }

  private def runFrontend(ctx: Context): List[CompilationUnit] = {
    try {
      val sources = ctx.files.toList.map(FileSource(_))
      FrontEnd.execute(ctx)(sources)
    } catch {
      case e: CompilationException =>
        print(e.messages.formattedWarnings)
        print(e.messages.formattedErrors)
        sys.exit(1)
    }
  }

  private def createContext(options: Options): Context = {
    val formatting = options.formatting
    Context(
      reporter = DefaultReporter(
        suppressWarnings = options(SuppressWarnings),
        warningIsError = options(WarningIsError),
        formatting = formatting,
        maxErrors = options(MaxErrors),
        errorContext = options(ErrorContext)
      ),
      errorContext = options(ErrorContext),
      files = options.files,
      classPath = ClassPath.Default ++ options.classPaths,
      outDirs = options.outDirectories,
      printCodePhase = options(PrintOutput),
      printInfo = options(Verbose),
      ignoredImports = options(IgnoreDefaultImports),
      formatting = formatting
    )
  }

  private def printFilesToCompile(ctx: Context) = {
    import ctx.formatting._
    val numFiles = ctx.files.size
    val files = ctx.files.map(formatFileName).mkString("\n")
    val end = if (numFiles > 1) "files" else "file"
    val header = Bold("Compiling") + " " + Magenta(numFiles) + " " + Bold(end)

    print(makeBox(header, List(files)))
  }

  private def printExecutionTimes(ctx: Context) = {
    import ctx.formatting._
    val totalTime = ctx.executionTimes.values.sum
    val individualTimes = CompilerPhases.map { phase =>
      val name = Blue(phase.name.capitalize)
      val time = ctx.executionTimes(phase)
      val t = Green(f"$time%.2f$Reset")
      f"$name%-25s $t s"
    }.mkString("\n")

    val header =
      f"${ Bold }Compilation executed ${ Green("successfully") }$Bold in $Green$totalTime%.2f$Reset ${ Bold }seconds.$Reset"
    print(makeBox(header, List(individualTimes)))
  }

  private def versionInfo = s"T-Compiler $VersionNumber"

  private def printHelp(options: Options) = {
    val formatting = options.formatting

    if (options(Version)) {
      print(versionInfo)
      sys.exit()
    }

    if (options(Phases)) {
      print(phaseInfo(options.formatting))
      sys.exit()
    }

    val args = options(Help)
    if (args.contains("")) {
      print(helpInfo(formatting))
      sys.exit()
    }

    args.foreach(Flag.get(_) ifDefined { flag =>
      print(flagInfo(flag, formatting))
    })

    if (args.nonEmpty) {
      sys.exit()
    }
  }

  private def flagInfo(flag: Flag, formatting: Formatting) = {
    import formatting._

    val header = flag.flagName(formatting)
    makeBox(header, List(flag.extendedDescription(formatting)))
  }

  private def helpInfo(formatting: Formatting) = {
    import formatting._

    val tcomp = Green("tcomp")
    val options = Blue("options")
    val source = Blue("source files")
    val header = s"> $tcomp <$options> <$source> \n\n" + Bold(Magenta("Options"))

    val flags = Flag.All.map { flag => (flag.flagName(formatting), flag.description(formatting)) }
    makeBoxWithColumn(header, flags)
  }

  private def phaseInfo(formatting: Formatting) = {
    import formatting._
    val header = Bold(s"Phases of the T-Compiler")
    val phases = CompilerPhases.map { phase =>
      (Magenta(phase.name.capitalize), phase.description(formatting))
    }
    makeBoxWithColumn(header, phases)
  }

  private def isValidTHomeDirectory(path: String): Boolean = {
    // TODO: Make this properly check that the directory is valid
    /*
    def listFiles(f: File): Array[File] = {
      val these = f.listFiles
      if (these == null)
        return Array[File]()
      these ++ these.filter(_.isDirectory).flatMap(listFiles)
    }

    val files = listFiles(new File(path))
    val neededFiles = List(
      "T",
      "T/lang",
      "T/lang/Object.t",
      "T/lang/String.t",
      "T/std"
    )
    val fileMap = mutable.Map() ++ neededFiles.map((_, false))
    val filePaths = files.map(_.getAbsolutePath.drop(path.length + 1).replaceAll("\\\\", "/"))
    for (f <- filePaths)
      fileMap(f) = true

    if (fileMap.exists(!_._2))
      return false
    */
    true

  }

  private def executeProgram(ctx: Context, cus: List[CompilationUnit]): Unit = {
    import ctx.formatting._

    val mainMethods = cus.flatMap(_.classes.flatMap(_.methods.filter(_.isMain)))
    if (mainMethods.isEmpty) {
      println("--exec failed, none of the given files contains a main method.")
      return
    }

    val header = if (mainMethods.size > 1) "Executing programs" else "Executing program"


    val programExecutor = ProgramExecutor()
    val outputBlocks = cus.flatMap { cu =>
      val file = cu.source.asInstanceOf[FileSource].file
      val output = syntaxHighlighter(programExecutor(ctx, file))
      center(formatFileName(file)) :: output :: Nil
    }
    print(makeBox(Bold(header), outputBlocks))
  }

}
