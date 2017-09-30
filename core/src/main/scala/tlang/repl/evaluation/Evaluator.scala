package tlang.repl.evaluation

import better.files.File
import tlang.Context
import tlang.compiler.analyzer.{Flowing, Naming, Typing}
import tlang.compiler.ast.Parsing
import tlang.compiler.ast.Trees._
import tlang.compiler.code.{CodeGeneration, Lowering}
import tlang.compiler.lexer.Lexing
import tlang.compiler.modification.Templating
import tlang.utils.Extensions._
import tlang.utils.{ProgramExecutor, StringSource}

object Evaluator {

  val ClassName        = "ReplExecution"
  val ReplOutputMarker = "__ReplRes__"
  val PrintMarker      = Print(StringLit(ReplOutputMarker))
  val ReplClassID      = ClassID(ClassName)

}

case class Evaluator(
  ctx: Context,
  extractor: Extractor,
  programExecutor: ProgramExecutor,
  replStatementTransformer: StatementTransformer,
  state: ReplState) {

  import Evaluator._

  private val parse    = Lexing andThen Parsing
  private val frontEnd = Templating andThen Naming andThen Typing andThen Flowing
  private val compile  = Lowering andThen CodeGeneration


  def apply(command: String): String = {
    state.clearStatements()
    val parsedInput = parseInput(command)
    val definitionMessages = extractor(parsedInput)
    val cus = compile(state.compilationUnit)
    val executionMessages = execute(cus)
    resultMessage(definitionMessages, executionMessages)
  }

  private def compile(CU: CompilationUnit): List[CompilationUnit] = {
    val allCUs = frontEnd.execute(ctx)(CU :: Nil)

    // Templating can generate additional CU's. We extract the one with REPL-class
    // and transform that one.
    val replCU = allCUs.find { _.classes.exists(_.tpe == ReplClassID) }.get
    val rest = allCUs.remove(replCU)

    val transformed: CompilationUnit = replStatementTransformer(replCU)
    transformed :: rest
  }

  private def execute(cus: List[CompilationUnit]): Iterator[String] = {
    compile.execute(ctx)(cus)

    val classFile = File(ctx.outDirs.head, Evaluator.ClassName + ".class")
    val res = programExecutor(classFile)
    state.addStatementsToHistory()

    getOutput(res).lines
  }

  private def resultMessage(definitionMessages: List[String], executionMessages: Iterator[String]): String = {
    val sb = new StringBuilder
    definitionMessages.foreach(sb ++= _ + NL)
    executionMessages.foreach(sb ++= _ + NL)
    sb.toString.trimWhiteSpaces
  }


  private def getOutput(s: String): String = {
    val start = s.indexOf(ReplOutputMarker)
    val end = s.lastIndexOf(ReplOutputMarker)
    if (start != -1 && end != -1) s.slice(start + ReplOutputMarker.length, end) else ""
  }


  private def parseInput(command: String): CompilationUnit = {
    val input = StringSource(command, ClassName) :: Nil
    parse.execute(ctx)(input).head
  }


}
