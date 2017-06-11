package tlang.compiler

import cafebabe.StackTrace
import tlang.Context
import tlang.compiler.analyzer.{FlowAnalysis, NameAnalysis, TypeChecking}
import tlang.compiler.ast.Parser
import tlang.compiler.ast.Trees.CompilationUnit
import tlang.compiler.code.{CodeGeneration, Desugaring}
import tlang.compiler.lexer.{Lexer, Token}
import tlang.compiler.modification.Templates
import tlang.utils.Extensions._

abstract class Pipeline[F, T] {
  self =>

  val compilerStageName: String = getClass.getSimpleName.dropRight(1).toLowerCase
  protected def run(ctx: Context)(v: List[F]): List[T]

  def andThen[G](thenn: Pipeline[T, G]): Pipeline[F, G] = new Pipeline[F, G] {
    def run(ctx: Context)(v: List[F]): List[G] = {
      val first = self.execute(ctx)(v)
      val second = thenn.execute(ctx)(first)
      second
    }
  }

  def execute(ctx: Context)(v: List[F]): List[T] = {
    val (output, time) = measureTime { run(ctx)(v) }
    if (Main.CompilerStages.contains(this)) {
      if (!ctx.executionTimes.contains(this))
        ctx.executionTimes += this -> time
      if (compilerStageName in ctx.printCodeStages)
        OutputPrinter(ctx).printCode(this, output)
    }
    ctx.reporter.terminateIfErrors()
    output
  }

  private case class OutputPrinter(ctx: Context) {

    import ctx.formatting._

    private val tabWidth = 2

    private def header = Bold("Output after ") + Blue(compilerStageName.capitalize)

    def printCode(stage: Pipeline[_, _], output: List[T]): Unit = stage match {
      case Lexer          => printTokens(output.asInstanceOf[List[List[Token]]])
      case Parser
           | Templates
           | NameAnalysis
           | TypeChecking
           | FlowAnalysis
           | Desugaring   => printASTs(output.asInstanceOf[List[CompilationUnit]])
      case CodeGeneration => printStackTraces(output.asInstanceOf[List[StackTrace]])
      case _              =>
    }

    private def printStackTraces(stackTraces: List[StackTrace]) = {
      val legend = s"$Bold%-6s %-6s %-7s %-15s%s$Reset"
        .format("Line", "PC", "Height", "ByteCode", "Info")

      val blocks = stackTraces.flatMap { st =>
        val header = center(st.header) + "\n\n" + legend
        header :: st.content :: Nil
      }

      print(makeBox(header, blocks))
    }

    private def printTokens(allTokens: List[List[Token]]) = {
      val legend = s"$Bold%-35s %-16s %s$Reset".format("Text", "Token", "Position")
      val blocks = allTokens.flatMap { tokens =>
        val source = tokens.head.source
        val header = center(formatFileName(source.mainName)) + "\n\n" + legend
        val body = tokens.map(formatToken).mkString("\n")
        header :: body :: Nil
      }
      print(makeBox(header, blocks))
    }

    private def formatToken(token: Token) = {
      val tokenName = token.kind.getClass.getSimpleName.dropRight(1).replaceAll("KIND", "")
      val text = token.toString
      val trimmed = if (text.charCount >= 35) text.takeChars(31) + "..." else text
      val start = NumColor(token.line) + ":" + NumColor(token.col)
      val end = NumColor(token.endLine) + ":" + NumColor(token.endCol)
      val pos = s"$start - $end"
      s"$Blue%-35s$Reset $Bold%-16s$Reset %s".format(trimmed, tokenName, pos)
    }

    private def printASTs(cus: List[CompilationUnit]) = {
      val mediumHeaderColor = Blue + Bold
      val blocks = cus.flatMap { cu =>
        val printedTree = treePrinter(cu).trimWhiteSpaces
        center(formatFileName(cu.source.mainName)) ::
        center(mediumHeaderColor("Pretty printed code")) + "\n\n" +
        prettyPrinter(cu).replaceAll("\t", " " * tabWidth).trimWhiteSpaces ::
        center(mediumHeaderColor("Formatted AST")) + "\n\n" + treePrinter.header ::
        printedTree ::
        Nil
      }
      print(makeBox(header, blocks))
    }
  }
}
