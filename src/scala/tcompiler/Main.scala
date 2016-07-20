package tcompiler

import java.io.File
import java.nio.file.{InvalidPathException, Paths}

import tcompiler.analyzer.{NameAnalysis, TypeChecking}
import tcompiler.ast.Trees._
import tcompiler.ast.{ASTBuilder, Parser, Printer}
import tcompiler.code.{CodeGeneration, Desugaring}
import tcompiler.imports.ImportMap
import tcompiler.lexer.Lexer
import tcompiler.modification.Templates
import tcompiler.utils._

import scala.collection.mutable
import scala.sys.process._

object Main extends MainErrors {

  import Flags._

  lazy val AllFlags = EnumerationMacros.sealedInstancesOf[Flag]

  val FileEnding    = ".kool"
  val VersionNumber = "0.0.1"
  val THome         = "T_HOME"
  val TLangObject   = "kool/lang/Object"
  val TLangString   = "kool/lang/String"

  def TDirectory = sys.env(THome)

  var Reporter: Reporter = null

  val flagActive = mutable.Map() ++ AllFlags.map(f => (f.flag, false))

  def main(args: Array[String]) {
    try {
      ctx = processOptions(args)

      val tDir = sys.env(THome)
      if (!isValidTHomeDirectory(tDir))
        FatalInvalidTHomeDirectory(tDir, THome)

      val frontEnd = Lexer andThen Parser andThen Templates andThen NameAnalysis andThen TypeChecking
      val compilation = Desugaring andThen CodeGeneration
      val cus = frontEnd.run(ctx)(ctx.files)

      if (flagActive(PrintGeneratedCode))
        cus.foreach(p => println(Printer(p)))


      if (ctx.reporter.hasWarnings)
        println(ctx.reporter.warningsString)

      compilation.run(ctx)(cus)

      if (flagActive(Exec))
        cus.foreach(executeProgram)
    } catch {
      case e: CompilationException => println(e.getMessage)
    }
  }

  private def executeProgram(cu: CompilationUnit): Unit = {
    if (!containsMainMethod(cu))
      return

    val cp = ctx.outDir match {
      case Some(dir) => "-cp " + dir.getPath
      case _         => ""
    }
    println("java " + cp + " " + fileName(cu) !!)
  }

  private def processOptions(args: Array[String]): Context = {
    var outDir: Option[String] = None
    var files: List[File] = Nil
    var classPaths: List[String] = List()
    var maxErrors = MaxErrors.DefaultMax

    def processOption(args: List[String]): Unit = args match {
      case Directory.flag :: out :: args                         =>
        outDir = Some(out)
        processOption(args)
      case ClassPath.flag :: dir :: args                         =>
        classPaths ::= dir
        processOption(args)
      case MaxErrors.flag :: num :: args                         =>
        try {
          maxErrors = num.toInt
        } catch {
          case e: NumberFormatException =>
            Reporter = new Reporter()
            FatalInvalidMaxErrors(num)
        }
        processOption(args)
      case Version.flag :: _                                     =>
        printVersion()
        sys.exit
      case Help.flag :: _                                        =>
        printHelp()
        sys.exit
      case flag :: args if flagActive.contains(flag.toLowerCase) =>
        flagActive(flag.toLowerCase) = true
        processOption(args)
      case f :: args                                             =>
        files = new File(f) :: files
        processOption(args)
      case Nil                                                   =>
    }


    if (args.isEmpty) {
      printHelp()
      sys.exit
    }


    processOption(args.toList)

    Reporter = new Reporter(
      flagActive(SuppressWarnings),
      flagActive(WarningIsError),
      !flagActive(NoColor),
      maxErrors)

    if (files.size != 1)
      FatalWrongNumFilesGiven(files.length)

    val file = files.head
    if (!file.exists())
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

    val cp = TDirectory :: classPaths
    Context(Reporter, files, cp, dir)
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
      // TODO: delete doesnt delete parent directories
      f.delete()
      if (canCreate)
        return false
    }
    true
  }

  private def printVersion() = println(s"T-Compiler $VersionNumber")

  private def printHelp() = println(
    s"""
       |Usage: tcomp <options> <source file>
       |Options:
       |
       |${AllFlags.map(_.format).mkString}
    """.stripMargin
  )

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

  def listFiles(f: File): Array[File] = {
    val these = f.listFiles
    if (these == null)
      return Array[File]()
    these ++ these.filter(_.isDirectory).flatMap(listFiles)
  }

  private def fileName(cu: CompilationUnit) = cu.file.getName.dropRight(FileEnding.length)

  private def containsMainMethod(cu: CompilationUnit) = cu.classes.exists(_.methods.exists(_.isMain))

}