package tlang
package filetester

import better.files.File
import tlang.compiler.Context
import tlang.compiler.lexer.Lexing
import tlang.compiler.lexer.Tokens.COMMENTLIT
import tlang.compiler.messages.CompilerMessage
import tlang.utils.{ExecutionResult, FileSource}

import scala.util.Try
import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

case class Solution(line: Int, content: String)
case class SolutionParserResult(expectedCodes: IndexedSeq[Solution], expectedOutput: IndexedSeq[Solution])

object SolutionParser {
  private val SolutionRegex: Regex = """// *[R|r]es:(.*)""".r
  private val LineSolutionRegex: Regex = (""".*""" + SolutionRegex).r
  private val OutputRegex: Regex = """(.*):(\d+)$""".r
  private val ErrorCodeRegex: Regex = """[A-Z]\d{4}.*""".r
}

case class SolutionParser(ctx: Context) {

  import SolutionParser._

  // We try read the solution using the Lexer first since this
  // works better (doesn't pickup comments within other comments etc.)
  // If that fails (for instance when testing lexing errors) we
  // fall back to parsing each line using a regex
  def parse(file: File): SolutionParserResult = {
    val solutions = Try(parseSolutionsWithLexer(file)).getOrElse(parseSolutionsWithRegex(file))
    val (expectedCodes, expectedOutput) = solutions.partition { solution =>
      ErrorCodeRegex.matches(solution.content)
    }

    SolutionParserResult(
      expectedCodes.sortBy { lineThenContent },
      expectedOutput.sortBy { line }
    )
  }

  def parse(executionResult: ExecutionResult): IndexedSeq[Solution] = {
    executionResult.output
      .lines
      .map { case OutputRegex(output, lineNumber) => Solution(lineNumber.toInt, output) }
      .toIndexedSeq
      .sortBy { line } // This sorting has to be stable
  }

  def parse(messages: List[CompilerMessage]): IndexedSeq[Solution] = {
    messages
      .map { msg => Solution(msg.pos.line, msg.code) }
      .sortBy { lineThenContent }
      .toIndexedSeq
  }

  private def parseSolutionsWithLexer(file: File): IndexedSeq[Solution] = {
    Lexing.execute(ctx)(FileSource(file) :: Nil)
      .flatten
      .flatMap {
        case comment@COMMENTLIT(SolutionRegex(value)) => parseSolution(comment.line, value.trim)
        case _                                        => Nil
      }
      .toIndexedSeq
  }

  private def parseSolutionsWithRegex(file: File): IndexedSeq[Solution] = {
    FileSource
      .getText(file)
      .lines
      .zipWithIndex
      .flatMap {
        case (LineSolutionRegex(line), lineNumber) => parseSolution(lineNumber + 1, line.trim)
        case _                                     => Nil
      }
      .toIndexedSeq
  }

  private def line(solution: Solution) = solution.line
  private def lineThenContent(solution: Solution) = (solution.line, solution.content)

  private def parseSolution(lineNumber: Int, solution: String): List[Solution] = {
    LineParser(solution).map { res => Solution(lineNumber, res) }
  }

  object LineParser extends RegexParsers {
    def apply(content: String): List[String] = parseAll(full, content).getOrElse(Nil)

    private def full: Parser[List[String]] = element ~ ("," ~> element).* ^^ { case element ~ elements =>
      element :: elements
    }

    private def element: Parser[String] = quoted | text

    private def quoted: Parser[String] = """".*?"""".r ^^ { s => s.substring(1, s.length - 1) }
    private def text: Parser[String] = """[^,]+""".r ^^ { _.trim }
  }

}
