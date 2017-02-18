package tlang.repl

import java.io.File

import tlang.compiler.Context
import tlang.compiler.analyzer.Symbols.ClassSymbol
import tlang.compiler.ast.Trees._
import tlang.compiler.code.CodeGeneration
import tlang.compiler.error.CompilationException
import tlang.compiler.imports.ImportMap
import tlang.compiler.main.Main
import tlang.compiler.utils.ProgramExecutor
import tlang.utils.StringSource

import scala.collection.mutable

/**
  * Created by Tim Lindeberg on 2/14/2017.
  */
case class ReplProgram() {

  val programExecutor = ProgramExecutor()

  val Pack = Package("T" :: "lang" :: Nil)

  val MainName          = "$ReplClass"
  val ClassName: String = Pack.mkString("::") + "::" + MainName
  val ReplClassID       = ClassID(ClassName)
  val ClassSymbol       = new ClassSymbol(ClassName, isAbstract = false)

  val TestFile = new File(ClassName.replaceAll("::", "/") + ".class")

  private val classes: mutable.Map[String, ClassDecl]  = mutable.Map()
  private val fields : mutable.Map[String, VarDecl]    = mutable.Map()
  private val methods: mutable.Map[String, MethodDecl] = mutable.Map()
  private var stat   : Option[StatTree]                = None

  def handleCommand(ctx: Context, command: String): String = {
    try {
      val input = StringSource(command, MainName) :: Nil
      Main.FrontEnd.run(ctx)(input)
    } catch {
      case e: CompilationException =>
        print(e.getMessage)
        sys.exit(1)
    }
    command
  }

  def executeReplProgram(ctx: Context): String = {
    if (stat.isEmpty)
      return ""

    val mainMethod = MethodDeclTree.mainMethod(stat, ClassSymbol)
    val mainClass = ClassDecl(ReplClassID, Nil, fields.values.toList, mainMethod :: methods.values.toList)
    val cu = CompilationUnit(Pack, mainClass :: classes.values.toList, new ImportMap(ctx))
    CodeGeneration.run(ctx)(cu :: Nil)

    programExecutor(TestFile) match {
      case Some(res) => res
      case None      => "Timed out!"
    }
  }

}
