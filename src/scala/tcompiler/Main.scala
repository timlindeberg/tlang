package tcompiler

import java.io.{File, FileNotFoundException}

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
  override val description = "Pretty prints the AST as it looks before analysis takes place."
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
  override val flag = "-nocolor"
  override val description = "Prints error messages without ANSI-coloring."
}

object Main {

  lazy val Flags = EnumerationMacros.sealedInstancesOf[Flag]

  val flagActive = mutable.Map() ++ Flags.map(f => (f.flag, false))

  var FileEnding    = ".kool"
  var VersionNumber = "0.0.1"


  def main(args: Array[String]) {
    try {
      val ctx = processOptions(args)

      val parsing = Lexer andThen Parser andThen Templates andThen Imports
      val analysis = NameAnalysis andThen TypeChecking
      // Generate code
      val preProg = parsing.run(ctx)(ctx.file)


      val prog = analysis.run(ctx)(preProg)
      if (flagActive(PrintGeneratedCode))
        println(Printer(preProg))
      CodeGeneration.run(ctx)(prog)
      if (flagActive(Exec) && containsMainMethod(prog)) {
        val cp = ctx.outDir match {
          case Some(dir) => "-cp " + dir.getPath
          case _         => ""
        }
        println("java " + cp + " " + fileName(ctx) !!)
      }
      if (ctx.reporter.hasWarnings)
        println(ctx.reporter.warningsString)

      System.out.flush()
    } catch {
      case e: CompilationException =>
        println(e.getMessage)
      // Reporter throws exception at fatal instead exiting program
      case e: FileNotFoundException => System.err.println("Error: File not found!")
    }
  }

  private def processOptions(args: Array[String]): Context = {
    var outDir: Option[File] = None
    var files: List[File] = Nil

    def processOption(args: List[String]): Unit = args match {
      case Directory.flag :: out :: args                         =>
        outDir = Some(new File(out))
        processOption(args)
      case Version.flag :: _                               =>
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

    val reporter = new Reporter(flagActive(SuppressWarnings), flagActive(WarningIsError), !flagActive(NoColor))

    if (files.size != 1)
      reporter.fatal("M", 0, s"Exactly one file expected, '${files.size}' file(s) given.")

    Context(reporter = reporter, file = files.head, outDir = outDir)
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

  private def fileName(ctx: Context) = ctx.file.getName.dropRight(FileEnding.length)

  private def containsMainMethod(program: Program) = program.classes.exists(_.methods.exists(_.isMain))
}