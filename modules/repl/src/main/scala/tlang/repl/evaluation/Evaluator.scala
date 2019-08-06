package tlang
package repl
package evaluation

import java.lang.reflect.InvocationTargetException

import better.files.File
import tlang.compiler.Context
import tlang.compiler.analyzer.{Flowing, Naming, Typing}
import tlang.compiler.ast.Parsing
import tlang.compiler.ast.Trees._
import tlang.compiler.code.{CodeGeneration, Lowering}
import tlang.compiler.lexer.Lexing
import tlang.compiler.modification.Templating

import tlang.utils.{Logging, ProgramExecutor, StringSource}


object Evaluator {


  val ClassName        = "ReplExecution"
  val ReplOutputMarker = "__ReplRes__"
  val PrintMarker      = Print(StringLit(ReplOutputMarker))
  val ReplClassID      = ClassID(ClassName)


  def apply(ctx: Context,
    extractor: Extractor,
    programExecutor: ProgramExecutor,
    saveAndPrintTransformer: SaveAndPrintTransformer,
    state: ReplState): Evaluator = {
    val parser = Lexing andThen Parsing
    val analyzer = Templating andThen Naming andThen Typing andThen Flowing
    val compiler = Lowering andThen CodeGeneration

    val classFile = File(ctx.outDirs.head, Evaluator.ClassName + ".class")
    Evaluator(
      classFile,
      extractor,
      programExecutor,
      saveAndPrintTransformer,
      state,
      parse = input => parser.execute(ctx)(input),
      analyze = cus => analyzer.execute(ctx)(cus),
      compile = cus => compiler.execute(ctx)(cus)
    )
  }
}

case class Evaluator(
  classFile: File,
  extractor: Extractor,
  programExecutor: ProgramExecutor,
  saveAndPrintTransformer: SaveAndPrintTransformer,
  state: ReplState,
  parse: List[StringSource] => List[CompilationUnit],
  analyze: List[CompilationUnit] => List[CompilationUnit],
  compile: List[CompilationUnit] => Unit
) extends Logging {

  import Evaluator._


  def apply(command: String): String = {
    debug"Clearing statements $command: $state"
    state.clearStatements()
    val parsedInput = parseInput(command)
    val definitionMessages = extractor(parsedInput)
    val cus = compile(state.compilationUnit)
    val executionMessages = execute(cus)
    debug"Finished evaluating"

    resultMessage(definitionMessages, executionMessages)
  }

  private def parseInput(command: String): CompilationUnit = {
    debug"Parsing input"
    val input = StringSource(command, ClassName) :: Nil
    parse(input).head
  }

  private def compile(CU: CompilationUnit): List[CompilationUnit] = {
    debug"Analyzing etc"
    val allCUs = analyze(CU :: Nil)

    // Templating can generate additional CU's. We extract the one with REPL-class
    // and transform that one.
    val replCU = allCUs.find { _.classes.exists { _.id == ReplClassID } }.get
    val rest = allCUs.remove(replCU)

    val transformed: CompilationUnit = saveAndPrintTransformer(replCU)
    transformed :: rest
  }

  private def execute(cus: List[CompilationUnit]): Iterator[String] = {
    debug"Compiling: $cus"

    compile(cus)

    debug"Executing program $classFile"
    val res = programExecutor(classFile)
    res.exception.ifDefined { e => throw new InvocationTargetException(e) }
    state.addStatementsToHistory()

    getOutput(res.output).lines
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


}
