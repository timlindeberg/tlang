package tlang
package compiler
package testutils

import better.files.File
import tlang.compiler.lexer.Lexing
import tlang.compiler.lexer.Tokens.COMMENTLIT
import tlang.utils.FileSource

import scala.util.Try
import scala.util.matching.Regex

object Solution {
  def unapply(arg: Solution): Option[(Int, String)] = Some((arg.line, arg.output))
}
trait Solution {
  def line: Int
  def output: String
}

case class ErrorMessageSolution(override val line: Int, errorCode: String) extends Solution {
  val output: String = errorCode
}
case class OutputSolution(override val line: Int, override val output: String) extends Solution

object SolutionParser {
  private val SolutionRegex: Regex = """// *[R|r]es:(.*)""".r
  private val LineSolutionRegex: Regex = (""".*""" + SolutionRegex).r
  private val ErrorCodeRegex: Regex = """[A-Z]\d{4}.*""".r
}

case class SolutionParser(ctx: Context) {

  import SolutionParser._

  // We try read the solution using the Lexer first since this
  // works better (doesn't pickup comments within other comments etc.)
  // If that fails (for instance when testing lexing errors) we
  // fall back to parsing each line using a regex
  def parse(file: File): List[Solution] = {
    Try(parseSolutionsWithLexer(file))
      .getOrElse(parseSolutionsWithRegex(file))
  }

  private def parseSolutionsWithLexer(file: File): List[Solution] = {
    Lexing.execute(ctx)(FileSource(file) :: Nil)
      .flatten
      .flatMap {
        case comment@COMMENTLIT(SolutionRegex(value)) => parseLine(comment.line, value.trim)
        case _                                        => Nil
      }
  }

  private def parseSolutionsWithRegex(file: File): List[Solution] = {
    FileSource
      .getText(file)
      .lines
      .zipWithIndex
      .flatMap {
        case (LineSolutionRegex(line), lineNumber) => parseLine(lineNumber + 1, line.trim)
        case _                                     => Nil
      }
      .toList
  }

  private def parseLine(lineNumber: Int, line: String): List[Solution] = {
    if (ErrorCodeRegex.matches(line))
      line.split(",").map(res => ErrorMessageSolution(lineNumber, res.trim)).toList
    else
      OutputSolution(lineNumber, line) :: Nil
  }
}
