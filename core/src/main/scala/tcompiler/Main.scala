package tcompiler

import tcompiler.analyzer.{FlowAnalysis, NameAnalysis, TypeChecking}
import tcompiler.ast.Trees._
import tcompiler.ast.{Parser, PrettyPrinter}
import tcompiler.code.{CodeGeneration, Desugaring}
import tcompiler.error.Formats.{Light, Simple}
import tcompiler.error.{CompilationException, DefaultReporter, Formats, Formatting}
import tcompiler.lexer.Lexer
import tcompiler.modification.Templates
import tcompiler.utils.Extensions._
import tcompiler.utils._

import scala.sys.process._

object Main extends MainErrors {

  import Flags._

  val FileEnding           = ".kool"
  val VersionNumber        = "0.0.1"
  val THome                = "T_HOME"
  val JavaObject           = "java/lang/Object"
  val JavaString           = "java/lang/String"
  val TExtensionAnnotation = "kool/lang/$ExtensionMethod"
  val colorizer            = new Colorizer(false)

  import colorizer._

  lazy val TDirectory: String = {
    if (!sys.env.contains(THome))
      FatalCantFindTHome(THome)
    sys.env(THome)
  }

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
    if (args.isEmpty) {
      printHelp()
      sys.exit(1)
    }

    if (!isValidTHomeDirectory(TDirectory))
      FatalInvalidTHomeDirectory(TDirectory, THome)

    val options = new Options(args)
    val formatType = Formats.Types
      .find(_.name in options(Flags.Formatting))
      .getOrElse(Light)


    colorizer.useColor = formatType != Simple
    val formatting = error.Formatting(formatType, colorizer)
    if (options(Version)) {
      printVersion()
      sys.exit()
    }
    if (options(Help).nonEmpty) {
      printHelp(options(Help))
      sys.exit()
    }
    if (options.files.isEmpty)
      FatalNoFilesGiven()

    ctx = createContext(options, formatting)

    if (options(PrintInfo))
      printFilesToCompile(ctx)

    val cus = runFrontend(ctx)

    val compilation = Desugaring andThen CodeGeneration
    compilation.run(ctx)(cus)

    if (ctx.reporter.hasWarnings)
      println(ctx.reporter.warningsString)

    if (options(PrintInfo))
      printExecutionTimes(ctx)

    if (options(Exec))
      cus.foreach(executeProgram)
  }

  private def runFrontend(ctx: Context): List[CompilationUnit] = {
    val frontEnd = Lexer andThen Parser andThen Templates andThen
      NameAnalysis andThen TypeChecking andThen FlowAnalysis

    val cus = try {
      frontEnd.run(ctx)(ctx.files)
    } catch {
      case e: CompilationException =>
        println(e.getMessage)
        sys.exit(1)
    }

    cus
  }

  private def createContext(options: Options, formatting: Formatting): Context =
    Context(
      reporter = new DefaultReporter(
        suppressWarnings = options(SuppressWarnings),
        warningIsError = options(WarningIsError),
        maxErrors = options.maxErrors,
        errorContext = options.errorContext,
        formatting = formatting
      ),
      files = options.files,
      classPaths = options.classPaths,
      outDirs = options.outDirectories,
      printCodeStages = options(PrintOutput),
      printInfo = options(PrintInfo),
      ignoredImports = options(IgnoreDefaultImports),
      colorizer = colorizer,
      printer = new PrettyPrinter(colorizer)
    )

  private def printFilesToCompile(ctx: Context) = {
    import ctx.colorizer._

    val numFiles = ctx.files.size
    val files = ctx.files.map { f =>
      val name = f.getName.dropRight(Main.FileEnding.length)
      val full = s"${Magenta(name)}${Main.FileEnding}"
      s"   <$full>"
    }.mkString("\n")
    val msg =
      s"""|${Bold("Compiling")} ${Magenta(numFiles)} ${Bold("file(s)")}:
          |$files
          |""".stripMargin
    println(msg)
  }

  private def printExecutionTimes(ctx: Context) = {
    val totalTime = ctx.executionTimes.values.sum
    val individualTimes = CompilerStages.map { stage =>
      val name = Blue(stage.stageName.capitalize)
      val time = ctx.executionTimes(stage)
      val t = Green(f"$time%.2f$Reset")
      f"   $name%-25s $t seconds"
    }.mkString("\n")
    val msg =
      f"""|${Bold("Compilation executed")} ${Green("successfully")} ${Bold("in")} $Green$totalTime%.2f$Reset ${Bold("seconds.")}
          |Execution time for individual stages:
          |$individualTimes
          |""".stripMargin
    println(msg)
  }

  private def printVersion() = println(s"T-Compiler $VersionNumber")

  private def printHelp(args: Set[String] = Set("")) = args foreach { arg =>
    val message = arg match {
      case "stages" =>
        val stages = CompilerStages.map(stage => s"   <$Blue${stage.stageName.capitalize}$Reset>").mkString("\n")
        s"""|The compiler stages are executed in the following order:
            |
            |$stages
            |"""
      case ""       =>
        val flags = Flag.All.map(_.format(colorizer)).mkString
        s"""|Usage: tcomp <options> <source files>
            |Options:
            |
            |$flags
            |"""
      case _        => ???
    }
    print(message.stripMargin)
  }

  private def isValidTHomeDirectory(path: String): Boolean = {
    // TODO: Make this properly check that the directory is valid
    return true

    /*
    def listFiles(f: File): Array[File] = {
      val these = f.listFiles
      if (these == null)
        return Array[File]()
      these ++ these.filter(_.isDirectory).flatMap(listFiles)
    }

    val files = listFiles(new File(path))
    val neededFiles = List(
      "kool",
      "kool/lang",
      "kool/lang/Object.kool",
      "kool/lang/String.kool",
      "kool/std"
    )
    val fileMap = mutable.Map() ++ neededFiles.map((_, false))
    val filePaths = files.map(_.getAbsolutePath.drop(path.length + 1).replaceAll("\\\\", "/"))
    for (f <- filePaths)
      fileMap(f) = true

    if (fileMap.exists(!_._2))
      return false

    true
    */
  }

  private def executeProgram(cu: CompilationUnit): Unit = {
    if (!cu.classes.exists(_.methods.exists(_.isMain)))
      return

    import colorizer._

    val cp = s"-cp ${ctx.outDirs.head}"
    val mainName = cu.file.get.getName.dropRight(FileEnding.length)
    val execCommand = s"java $cp $mainName"
    val separator = Blue("----------------------------------------")

    print(
      s"""Executing main program ${Magenta(mainName)}:
         |$separator
         |${execCommand !!}
         |$separator
       """.stripMargin)
  }

}