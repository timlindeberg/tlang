package tlang.compiler

import tlang.compiler.analyzer.{FlowAnalysis, NameAnalysis, TypeChecking}
import tlang.compiler.ast.Parser
import tlang.compiler.ast.Trees._
import tlang.compiler.code.{CodeGeneration, Desugaring}
import tlang.compiler.error._
import tlang.compiler.imports.ClassSymbolLocator
import tlang.compiler.lexer.Lexer
import tlang.compiler.modification.Templates
import tlang.compiler.options.{Flags, Options}
import tlang.utils.formatting.Formatting
import tlang.utils.{FileSource, ProgramExecutor, Source}

object Main extends MainErrors {

  import Flags._


  val FileEnding                     = ".t"
  val VersionNumber                  = "0.0.1"
  val THome                          = "T_HOME"
  val JavaObject                     = "java::lang::Object"
  val JavaString                     = "java::lang::String"
  val TInt                           = "T::lang::Int"
  val TLong                          = "T::lang::Long"
  val TFloat                         = "T::lang::Float"
  val TDouble                        = "T::lang::Double"
  val TChar                          = "T::lang::Char"
  val TBool                          = "T::lang::Bool"
  val TExtensionAnnotation           = "T::lang::$ExtensionMethod"
  val TImplicitConstructorAnnotation = "T::lang::$ImplicitConstructor"

  val Primitives = List(TInt, TLong, TFloat, TDouble, TBool, TChar)

  lazy val TDirectory: String = {
    if (!sys.env.contains(THome))
      FatalCantFindTHome(THome)
    sys.env(THome)
  }

  val FrontEnd: Pipeline[Source, CompilationUnit] =
    Lexer andThen Parser andThen Templates andThen
    NameAnalysis andThen TypeChecking andThen FlowAnalysis andThen Desugaring

  val CompilerStages = List(
    Lexer,
    Parser,
    Templates,
    NameAnalysis,
    TypeChecking,
    FlowAnalysis,
    Desugaring,
    CodeGeneration
  )

  def main(args: Array[String]) {
    val options = Options(args)

    if (args.isEmpty) {
      printHelp(options.formatting)
      sys.exit(1)
    }

    checkTHome()

    if (options(Version)) {
      printVersion()
      sys.exit()
    }

    if (options(Help).nonEmpty) {
      printHelp(options.formatting, options(Help))
      sys.exit()
    }

    if (options.files.isEmpty)
      FatalNoFilesGiven()

    val ctx = createContext(options)
    ClassSymbolLocator.setClassPath(ctx.getClassPaths)

    if (options(Verbose))
      printFilesToCompile(ctx)

    val cus = runFrontend(ctx)

    CodeGeneration.run(ctx)(cus)

    if (ctx.reporter.hasWarnings)
      print(ctx.reporter.messages.formattedWarnings)


    if (options(Verbose))
      printExecutionTimes(ctx)

    if (options(Exec))
      executeProgram(ctx, cus)
  }

  def checkTHome(): Unit = {
    if (!isValidTHomeDirectory(TDirectory))
      FatalInvalidTHomeDirectory(TDirectory, THome)
  }

  private def runFrontend(ctx: Context): List[CompilationUnit] = {
    try {
      val sources = ctx.files.toList.map(FileSource)
      FrontEnd.run(ctx)(sources)
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
      classPaths = options.classPaths,
      outDirs = options.outDirectories,
      printCodeStages = options(PrintOutput),
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
    val individualTimes = CompilerStages.map { stage =>
      val name = Blue(stage.compilerStageName.capitalize)
      val time = ctx.executionTimes(stage)
      val t = Green(f"$time%.2f$Reset")
      f"$name%-25s $t s"
    }.mkString("\n")

    val header =
      f"${ Bold }Compilation executed ${ Green("successfully") }$Bold in $Green$totalTime%.2f$Reset ${ Bold }seconds.$Reset"
    print(makeBox(header, List(individualTimes)))
  }

  private def printVersion() = println(s"T-Compiler $VersionNumber")

  private def printHelp(formatting: Formatting, args: Set[String] = Set("")) = {
    args foreach { arg =>
      import formatting._

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


    val programExecutor = ProgramExecutor(timeout = None)
    val outputBlocks = cus.flatMap { cu =>
      val file = cu.source.asInstanceOf[FileSource].file
      val output = syntaxHighlighter(programExecutor(ctx, file))
      center(formatFileName(file)) :: output :: Nil
    }
    print(makeBox(Bold(header), outputBlocks))
  }

}