package tcompiler

import java.io.File
import java.nio.file.{InvalidPathException, Paths}

import tcompiler.analyzer.{FlowAnalysis, NameAnalysis, TypeChecking}
import tcompiler.ast.Parser
import tcompiler.ast.Trees._
import tcompiler.code.{CodeGeneration, Desugaring}
import tcompiler.lexer.Lexer
import tcompiler.modification.Templates
import tcompiler.utils._

import scala.collection.mutable
import scala.sys.process._

object Main extends MainErrors with Colored {

  import Flags._

  val FileEnding           = ".kool"
  val VersionNumber        = "0.0.1"
  val THome                = "T_HOME"
  val JavaObject           = "java/lang/Object"
  val JavaString           = "java/lang/String"
  val TExtensionAnnotation = "kool/lang/$ExtensionMethod"

  def TDirectory: String = {
    if (!sys.env.contains(THome))
      FatalCantFindTHome(THome)
    sys.env(THome)
  }

  override def useColor: Boolean = !flagActive(NoColor)

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

  // This has to be lazy because of some weird macro behaviour
  private lazy val AllFlags: Set[Flag] = EnumerationMacros.sealedInstancesOf[Flag]
  private      val flagActive          = mutable.Map() ++ AllFlags.map(f => (f.flag, false))

  def main(args: Array[String]) {
    try {
      ctx = processOptions(args)

      val tDir = sys.env(THome)
      if (!isValidTHomeDirectory(tDir))
        FatalInvalidTHomeDirectory(tDir, THome)

      if (flagActive(PrintInfo))
        printFilesToCompile(ctx)

      val frontEnd = Lexer andThen Parser andThen Templates andThen NameAnalysis andThen TypeChecking andThen FlowAnalysis
      val compilation = Desugaring andThen CodeGeneration
      val cus = frontEnd.run(ctx)(ctx.files)

      if (ctx.reporter.hasWarnings)
        println(ctx.reporter.warningsString)

      compilation.run(ctx)(cus)

      if (flagActive(PrintInfo))
        printExecutionTimes()

      if (flagActive(Exec))
        cus.foreach(executeProgram)
    } catch {
      case e: CompilationException => println(e.getMessage)
    }
  }

  private def processOptions(args: Array[String]): Context = {
    var outDir: Option[String] = None
    var files: List[File] = Nil
    var classPaths: List[String] = Nil
    var maxErrors = MaxErrors.DefaultMax
    var printStage: Option[String] = None

    def processOption(args: List[String]): Unit = args match {
      case Directory.flag :: out :: args                         =>
        outDir = Some(out)
        processOption(args)
      case ClassPath.flag :: dir :: args                         =>
        classPaths ::= dir
        processOption(args)
      case MaxErrors.flag :: num :: args                         =>
        maxErrors = parseNumber(num, FatalInvalidMaxErrors)
        processOption(args)
      case Version.flag :: _                                     =>
        printVersion()
        sys.exit
      case Help.flag :: arg                                      =>
        printHelp(arg)
        sys.exit
      case PrintCode.flag :: arg :: args                         =>
        val s = arg.toLowerCase
        val (stage, rest) = if (isValidStage(s))
          (s, args)
        else
          (Desugaring.stageName, arg :: args)
        printStage = Some(stage)
        processOption(rest)
      case flag :: args if flagActive.contains(flag.toLowerCase) =>
        flagActive(flag.toLowerCase) = true
        processOption(args)
      case filePath :: args                                      =>
        files :::= getFiles(filePath)
        processOption(args)
      case Nil                                                   =>
    }

    if (args.isEmpty) {
      printHelp()
      sys.exit
    }

    processOption(lowerCaseArgs(args))

    if (files.isEmpty)
      FatalNoFilesGiven()

    for (file <- files) if (!file.exists())
      FatalCannotFindFile(file.getPath)

    checkValidClassPaths(classPaths)

    val dir = outDir match {
      case Some(dir) =>
        if (!isValidPath(dir))
          FatalInvalidOutputDirectory(dir)
        Some(new File(dir))
      case None      => None
    }

    if (!sys.env.contains(THome))
      FatalCantFindTHome(THome)

    val reporter = new Reporter(flagActive(SuppressWarnings),
      flagActive(WarningIsError),
      !flagActive(NoColor),
      maxErrors)

    val cp = TDirectory :: classPaths
    Context(reporter, files, cp, dir, printStage, !flagActive(NoColor), flagActive(PrintInfo))
  }

  private def getFiles(path: String): List[File] = {
    val file = new File(path)
    if (file.isDirectory) {
      val tFiles = file.listFiles().filter(_.getName.endsWith(FileEnding))
      if (tFiles.isEmpty)
        FatalGivenDirectoryContainsNoTFiles(path)

      return tFiles.toList
    }

    if (!file.getName.endsWith(FileEnding))
      FatalGivenFileIsNotTFile(path)
    List(file)
  }

  private def lowerCaseArgs(args: Array[String]) =
    args.map(arg => {
      if (arg.contains(Main.FileEnding)) arg else arg.toLowerCase
    }).toList

  private def printFilesToCompile(ctx: Context) = {
    val numFiles = ctx.files.size
    val files = ctx.files.map { f =>
      val name = f.getName.dropRight(Main.FileEnding.length)
      val full = s"${Magenta(name) }${Main.FileEnding }"
      s"   <$full>"
    }.mkString("\n")
    val msg =
      s"""|${Bold("Compiling") } ${Magenta(numFiles) } ${Bold("file(s)") }:
          |$files
          |""".stripMargin
    println(msg)
  }

  private def isValidStage(stage: String) = CompilerStages.map(_.stageName).contains(stage)

  private def parseNumber(num: String, error: String => Nothing): Int =
    try {
      num.toInt
    } catch {
      case e: NumberFormatException => error(num)
    }

  private def checkValidClassPaths(classPaths: List[String]) =
    classPaths.find(!isValidPath(_)) collect {
      case path => FatalInvalidOutputDirectory(path)
    }

  private def isValidPath(path: String): Boolean = {
    try {
      Paths.get(path)
    } catch {
      case e: InvalidPathException =>
        return false
    }
    val f = new File(path)
    if (f.isFile)
      return false

    if (!f.exists()) {
      val canCreate = f.mkdirs()
      // TODO: delete doesn't delete parent directories
      f.delete()
      if (canCreate)
        return false
    }
    true
  }

  private def printExecutionTimes() = {
    val totalTime = ctx.executionTimes.values.sum
    val individualTimes = CompilerStages.map { stage =>
      val name = Blue(stage.stageName.capitalize)
      val time = ctx.executionTimes(stage)
      val t = Green(f"$time%.2f$Reset")
      f"   $name%-25s $t seconds"
    }.mkString("\n")
    val msg =
      f"""|${Bold("Compilation executed") } ${Green("successfully") } ${Bold("in") } $Green$totalTime%.2f$Reset ${Bold("seconds.") }
          |Execution time for individual stages:
          |$individualTimes
          |""".stripMargin
    println(msg)
  }

  private def printVersion() = println(s"T-Compiler $VersionNumber")

  private def printHelp(arg: List[String] = Nil) = {
    val helpMessage = arg match {
      case "stages" :: _ =>
        val stages = CompilerStages.map(stage => s"   <${stage.stageName.capitalize }>").mkString("\n")
        s"""|The compiler stages are executed in the following order:
            |$stages
            |""".stripMargin
      case _             =>
        val flags = AllFlags.map(_.format).mkString
        s"""|Usage: tcomp <options> <source files>
            |Options:
            |
            |$flags
            |""".stripMargin
    }
    var s = """\<(.+?)\>""".r.replaceAllIn(helpMessage, m => s"<${Blue(m.group(1)) }>")
    s = "-(.+?) ".r.replaceAllIn(s, m => s"-${Magenta(m.group(1)) } ")
    print(s)
  }

  private def isValidTHomeDirectory(path: String): Boolean = {
    // TODO: Make this properly check that the directory is valid
    return true

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
  }

  private def executeProgram(cu: CompilationUnit): Unit = {
    if (!containsMainMethod(cu))
      return

    val cp = ctx.outDir match {
      case Some(dir) => s"-cp ${dir.getPath }"
      case _         => ""
    }
    val mainName = cu.file.getName.dropRight(FileEnding.length)
    val execCommand = s"java $cp $mainName"
    val seperator = Blue("----------------------------------------")

    print(
      s"""Executing main program ${Magenta(mainName) }:
         |$seperator
         |${execCommand!! }
         |$seperator
       """.stripMargin)
  }

  private def listFiles(f: File): Array[File] = {
    val these = f.listFiles
    if (these == null)
      return Array[File]()
    these ++ these.filter(_.isDirectory).flatMap(listFiles)
  }

  private def containsMainMethod(cu: CompilationUnit) = cu.classes.exists(_.methods.exists(_.isMain))

}