package tlang.compiler

import cafebabe.StackTrace
import tlang.Context
import tlang.compiler.analyzer.{FlowAnalysis, NameAnalysis, TypeChecking}
import tlang.compiler.ast.Trees.CompilationUnit
import tlang.compiler.ast.{Parser, TreePrinter}
import tlang.compiler.code.{CodeGeneration, Desugaring}
import tlang.compiler.lexer.{Lexer, Token}
import tlang.utils.Extensions._

abstract class Pipeline[F, T] {
  self =>


  protected def run(ctx: Context)(v: List[F]): List[T]

  val compilerStageName: String = getClass.getSimpleName.dropRight(1).toLowerCase

  def andThen[G](thenn: Pipeline[T, G]): Pipeline[F, G] = new Pipeline[F, G] {
    def run(ctx: Context)(v: List[F]): List[G] = {
      val first = self.execute(ctx)(v)
      val second = thenn.execute(ctx)(first)
      second
    }
  }

  def execute(ctx: Context)(v: List[F]): List[T] = {
    val infoPrinter = OutputPrinter(ctx)
    val (output, time) = measureTime { run(ctx)(v) }
    if (Main.CompilerStages.contains(this)) {
      ctx.executionTimes += this -> time
      infoPrinter.printCode(this, output)
    }
    ctx.reporter.terminateIfErrors()
    output
  }

  private case class OutputPrinter(ctx: Context) {

    import ctx.formatting._

    private val stageNameCapitalized = compilerStageName.capitalize
    private val shouldPrint          = ctx.printCodeStages.contains(compilerStageName)
    private val header               = Bold("Output after ") + Blue(stageNameCapitalized)


    def printCode(stage: Pipeline[_, _], output: List[T]): Unit = {
      if (!shouldPrint)
        return

      stage match {
        case Lexer          => printTokens(output.asInstanceOf[List[List[Token]]])
        case Parser         => printASTs(output.asInstanceOf[List[CompilationUnit]])
        case NameAnalysis
             | TypeChecking
             | FlowAnalysis
             | Desugaring   => printCompilationUnits(output.asInstanceOf[List[CompilationUnit]])
        case CodeGeneration => printStackTraces(output.asInstanceOf[List[StackTrace]])
        case _              =>
      }
    }

    private def printCompilationUnits(cus: List[CompilationUnit]) = {
      val blocks = cus.flatMap { cu => center(formatFileName(cu.source.mainName)) :: prettyPrinter(cu).trimWhiteSpaces :: Nil }
      print(makeBox(header, blocks))
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
      val width = if (lineWidth <= 80) 1 else 2
      val treePrinter = TreePrinter(ctx.formatting, width)
      val blocks = cus.flatMap { cu => center(formatFileName(cu.source.mainName)) :: treePrinter(cu).trimWhiteSpaces :: Nil }
      print(makeBox(header, blocks))
    }
  }
}
