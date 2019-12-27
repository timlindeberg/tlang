package tlang.testutils.solutions

import better.files.File
import tlang.compiler.Context
import tlang.compiler.lexer.Lexing
import tlang.compiler.lexer.Tokens.COMMENTLIT
import tlang.utils.{ExecutionResult, FileSource}

import scala.util.Try
import scala.util.matching.Regex

object Solution {
  def unapply(arg: Solution): Option[(Int, String)] = Some((arg.line, arg.content))
}
trait Solution {
  def line: Int
  def content: String
}

case class ErrorMessageSolution(override val line: Int, errorCode: String) extends Solution {
  val content: String = errorCode
}
case class OutputSolution(override val line: Int, override val content: String) extends Solution

object SolutionParser {
  private val SolutionRegex: Regex = """// *[R|r]es:(.*)""".r
  private val LineSolutionRegex: Regex = (""".*""" + SolutionRegex).r
  private val ErrorCodeRegex: Regex = """[A-Z]\d{4}.*""".r
  private val OutputRegex = """(\d+): (.*)""".r
}

case class SolutionParser(ctx: Context) {

  import SolutionParser._

  // We try read the solution using the Lexer first since this
  // works better (doesn't pickup comments within other comments etc.)
  // If that fails (for instance when testing lexing errors) we
  // fall back to parsing each line using a regex
  def parse(file: File): IndexedSeq[Solution] = {
    Try(parseSolutionsWithLexer(file))
      .getOrElse(parseSolutionsWithRegex(file))
  }

  def parse(executionResult: ExecutionResult): IndexedSeq[Solution] = {
    executionResult.output
      .lines
      .map { case OutputRegex(lineNumber, output) => OutputSolution(lineNumber.toInt, output) }
      .toIndexedSeq
  }

  private def parseSolutionsWithLexer(file: File): IndexedSeq[Solution] = {
    Lexing.execute(ctx)(FileSource(file) :: Nil)
      .flatten
      .flatMap {
        case comment@COMMENTLIT(SolutionRegex(value)) => parseLine(comment.line, value.trim)
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
        case (LineSolutionRegex(line), lineNumber) => parseLine(lineNumber + 1, line.trim)
        case _                                     => Nil
      }
      .toIndexedSeq
  }

  private def parseLine(lineNumber: Int, line: String): List[Solution] = {
    if (ErrorCodeRegex.matches(line))
      line.split(",").map(res => ErrorMessageSolution(lineNumber, res.trim)).toList
    else
      OutputSolution(lineNumber, line) :: Nil
  }
}
