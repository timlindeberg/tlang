package tcompiler

import java.io.File
import java.nio.file.{InvalidPathException, Paths}

import tcompiler.analyzer.{NameAnalysis, TypeChecking}
import tcompiler.ast.Trees._
import tcompiler.ast.{Parser, Printer}
import tcompiler.code.CodeGeneration
import tcompiler.lexer.Lexer
import tcompiler.modification.{Imports, Templates}
import tcompiler.utils._

import scala.collection.mutable
import scala.sys.process._

// These have to be defined before the main object or the macro wont work
sealed abstract class Flag extends Ordered[Flag] with Product with Serializable {

  implicit def flag2String(f: Flag): String = f.flag

  val flag       : String
  val description: String
  val arg: String = ""
  def format: String = f"  ${flag + " " + arg}%-20s$description%-50s\n"
  def compare(that: Flag) = flag.length - that.flag.length
}

case object Exec extends Flag {
  override val flag        = "-exec"
  override val description = "Executes the program after compilation."
}

case object SuppressWarnings extends Flag {
  override val flag        = "-nowarn"
  override val description = "Suppresses warning messages."
}

case object PrintGeneratedCode extends Flag {
  override val flag        = "-printcode"
  override val description = "Pretty prints the AST as it looks before code is generated."
}

case object Help extends Flag {
  override val flag        = "-help"
  override val description = "Prints help information and exits."
}

case object Directory extends Flag {
  override val flag        = "-d"
  override val description = "Specify the path where generated classes are placed."
  override val arg         = "<directory>"
}

case object Version extends Flag {
  override val flag        = "-version"
  override val description = "Prints version information and exits."
}

case object WarningIsError extends Flag {
  override val flag        = "-Werror"
  override val description = "Treats warnings as errors and exits compilation."
}

case object NoColor extends Flag {
  override val flag        = "-nocolor"
  override val description = "Prints error messages without ANSI-coloring."
}

case object ClassPath extends Flag {
  override val flag        = "-cp"
  override val description = "Specify a path where classes should be searched for."
  override val arg         = "<directory>"
}

object Main {

  val FileEnding    = ".kool"
  val VersionNumber = "0.0.1"
  val THome         = "T_HOME"

  var TDirectory = ""
  var Ctx: Context = null
  var Reporter: Reporter = null

  lazy val Flags = EnumerationMacros.sealedInstancesOf[Flag]
  val flagActive = mutable.Map() ++ Flags.map(f => (f.flag, false))


  private val ErrorPrefix = "M"

  def main(args: Array[String]) {
    try {
      Ctx = processOptions(args)
      if (!sys.env.contains(THome))
        FatalCantFindTHome()

      TDirectory = sys.env(THome)

      if(!isValidTHomeDirectory(TDirectory))
        FatalInvalidTHomeDirectory(TDirectory)

      val parsing = Lexer andThen Parser andThen Templates andThen Imports
      val analysis = NameAnalysis andThen TypeChecking
      // Generate code
      val preProg = parsing.run(Ctx)(Ctx.file)


      val prog = analysis.run(Ctx)(preProg)
      if (flagActive(PrintGeneratedCode))
        println(Printer(prog))
      CodeGeneration.run(Ctx)(prog)
      if (Ctx.reporter.hasWarnings)
        println(Ctx.reporter.warningsString)

      if (flagActive(Exec))
        executeProgram(prog)
    } catch {
      case e: CompilationException  => println(e.getMessage)
    }
  }

  private def executeProgram(prog: Program): Unit = {
    if(!containsMainMethod(prog))
      return

    val cp = Ctx.outDir match {
      case Some(dir) => "-cp " + dir.getPath
      case _         => ""
    }
    println("java " + cp + " " + fileName(Ctx) !!)
  }

  private def processOptions(args: Array[String]): Context = {
    var outDir: Option[String] = None
    var files: List[File] = Nil
    var classPaths: List[String] = List()
    def processOption(args: List[String]): Unit = args match {
      case Directory.flag :: out :: args                         =>
        outDir = Some(out)
        processOption(args)
      case ClassPath.flag :: dir :: args =>
        classPaths ::= dir
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

      case Nil =>
    }

    if (args.isEmpty) {
      printHelp()
      sys.exit
    }


    processOption(args.toList)

    Reporter = new Reporter(flagActive(SuppressWarnings), flagActive(WarningIsError), !flagActive(NoColor))

    if (files.size != 1)
      FatalWrongNumFilesGiven(files.length)

    val file = files.head
    if(!file.exists())
      FatalCannotFindFile(file.getPath)

    checkValidClassPaths(classPaths)

    val dir = outDir match {
      case Some(dir) =>
        if(!isValidPath(dir))
          FatalInvalidOutputDirectory(dir)
        Some(new File(dir))
      case None => None
    }
    Context(Reporter, file, classPaths, dir)
  }

  private def checkValidClassPaths(classPaths: List[String]): Unit =
    for(path <- classPaths)
      if(!isValidPath(path))
        FatalInvalidOutputDirectory(path)


  private def isValidPath(path: String): Boolean = {
    try {
      Paths.get(path)
    }catch{
      case e: InvalidPathException =>
        return false
    }
    val f = new File(path)
    if(f.isFile)
      return false

    if(!f.exists()){
      val canCreate = f.mkdirs()
      // TODO: delete doesnt delete parent directories
      f.delete()
      if(canCreate)
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
         |${Flags.map(_.format).mkString}
    """.stripMargin
  )

  private def isValidTHomeDirectory(path: String): Boolean = {
    val files = listFiles(new File(path))
    println(path)
    val neededFiles = List(
      "kool",
      "kool/lang",
      "kool/lang/Object.kool",
      "kool/lang/String.kool",
      "kool/std"
    )
    val fileMap = mutable.Map() ++ neededFiles.map((_, false))
    for(f <- files.map(_.getAbsolutePath.drop(path.length + 1).replaceAll("\\\\", "/")))
      fileMap(f) = true

    for((f, found) <- fileMap)
      if(!found)
        return false

    true
  }

  def listFiles(f: File): Array[File] = {
    val these = f.listFiles
    if(these == null)
      return Array[File]()
    these ++ these.filter(_.isDirectory).flatMap(listFiles)
  }

  private def fileName(ctx: Context) = ctx.file.getName.dropRight(FileEnding.length)

  private def containsMainMethod(program: Program) = program.classes.exists(_.methods.exists(_.isMain))

  private def fatal(errorCode: Int, msg: String) =
    Reporter.fatal(ErrorPrefix, errorCode, msg)

  //---------------------------------------------------------------------------------------
  // Errors
  //---------------------------------------------------------------------------------------

  private def FatalWrongNumFilesGiven(numFiles: Int) =
    fatal(0, s"Exactly one file expected, '$numFiles' file(s) given.")

  private def FatalCannotFindFile(fileName: String) =
    fatal(1, s"Cannot find file '$fileName'.")

  private def FatalInvalidOutputDirectory(outDir: String) =
    fatal(2, s"Invalid output directory: '$outDir'.")

  private def FatalOutputDirectoryCouldNotBeCreated(outDir: String) =
    fatal(3, s"Output directory '$outDir' does not exist and could not be created.")

  private def FatalInvalidClassPath(classPath: String) =
    fatal(4, s"Invalid output class path: '$classPath'.")

  private def FatalCantFindTHome() =
    fatal(5, s"$THome environment variable is not set.")

  private def FatalInvalidTHomeDirectory(path: String) =
    fatal(6, s"'$path' is not a valid $THome directory.")
}