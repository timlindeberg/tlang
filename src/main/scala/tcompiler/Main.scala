package tcompiler

import java.io.{File, FileNotFoundException}

import tcompiler.analyzer.{NameAnalysis, TypeChecking}
import tcompiler.ast.Parser
import tcompiler.ast.Trees._
import tcompiler.code.CodeGeneration
import tcompiler.lexer.Lexer
import tcompiler.modification.{Imports, Templates}
import tcompiler.utils._

import scala.collection.mutable.HashMap
import scala.sys.process._

object Main {

  var FileEnding = ".kool"

  val exec       = "--exec"
  val suppressWarnings   = "-sw"
  val flags      = HashMap(
    exec -> false,
    suppressWarnings -> false
  )

  def processOptions(args: Array[String]): Context = {

    var outDir: Option[File] = None
    var files: List[File] = Nil

    def processOption(args: List[String]): Unit = args match {
      case "-d" :: out :: args =>
        outDir = Some(new File(out))
        processOption(args)

      case flag :: args if flags.contains(flag) =>
        flags(flag) = true
        processOption(args)

      case f :: args =>
        files = new File(f) :: files
        processOption(args)

      case Nil =>
    }

    processOption(args.toList)

    val reporter = new Reporter(flags(suppressWarnings))

    if (files.size != 1) reporter.fatal("M", 0, s"Exactly one file expected, '${files.size}' file(s) given.")

    Context(reporter = reporter, file = files.head, outDir = outDir)
  }

  def main(args: Array[String]) {
    try {
      val ctx = processOptions(args)

      val parsing = Lexer andThen Parser andThen Templates andThen Imports
      val analysis = NameAnalysis andThen TypeChecking
      // Generate code
      val prog = (parsing andThen analysis).run(ctx)(ctx.file)
      CodeGeneration.run(ctx)(prog)
      if (flags(exec) && containsMainMethod(prog)) {
        val cp = ctx.outDir match {
          case Some(dir) => "-cp " + dir.getPath
          case _         => ""
        }
        println("java " + cp + " " + fileName(ctx) !!)
      }
      if(ctx.reporter.hasWarnings)
        println(ctx.reporter.warningsString)

      System.out.flush()
    } catch {
      case e: CompilationException =>
        println(e.getMessage)
      // Reporter throws exception at fatal instead exiting program
      case e: FileNotFoundException => System.err.println("Error: File not found!")
    }
  }

  def fileName(ctx: Context) = ctx.file.getName.dropRight(FileEnding.length)

  private def containsMainMethod(program: Program) = program.classes.exists(_.methods.exists {
    case MethodDecl(Some(UnitType()), Identifier("main"), Formal(ArrayType(StringType()), _) :: Nil, _, mods) if mainModifiers(mods) => true
    case _ => false
  })

  private def mainModifiers(modifiers: Set[Modifier]) = modifiers.size == 2 && modifiers.contains(Public()) && modifiers.contains(Static())
}