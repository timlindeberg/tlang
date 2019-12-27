package tlang
package filetester

import tlang.formatting.{Colors, Formatter}

import scala.collection.mutable.ListBuffer

case class SolutionVerifier()(implicit formatter: Formatter) {

  private case class Result(result: Option[Solution], solution: Option[Solution], isMatch: Boolean)

  def apply(results: IndexedSeq[Solution], solutions: IndexedSeq[Solution], colorContent: Boolean): Unit = {
    val res = compare(results, solutions)

    val errors = res.filter { !_.isMatch }

    if (errors.isEmpty)
      return

    val message = makeErrorMessage(errors)
    val extraInfo = drawExtraInfo(res, results, solutions, colorContent)
    fail(message, extraInfo)
  }

  private def makeErrorMessage(errors: List[Result]): String = {
    errors
      .map {
        case Result(Some(Solution(line, content)), None, _)               => s"Unexpected '$content' on line $line"
        case Result(None, Some(Solution(line, content)), _)               => s"Expected '$content' on line $line"
        case Result(Some(Solution(line, res)), Some(Solution(_, sol)), _) => s"Expected '$sol' on line $line but found '$res'"
        case _                                                            => ???
      }
      .mkString(NL)
  }

  private def drawExtraInfo(res: List[Result], results: IndexedSeq[Solution], solutions: IndexedSeq[Solution], colorContent: Boolean): String = {
    val maxLineWidth = (results ++ solutions).map { _.line.digits }.max

    val grid = formatter.grid
      .row(2)
      .columnHeaders("Result", "Solution")

    res.foreach { case Result(res, sol, isMatch) =>
      val result = format(res, isMatch, colorContent, maxLineWidth)
      val solution = format(sol, isMatch, colorContent, maxLineWidth)
      grid.content(result, solution)
    }
    grid.render()
  }

  private def compare(results: IndexedSeq[Solution], solutions: IndexedSeq[Solution]): List[Result] = {
    val comparison = ListBuffer[Result]()

    var resIndex = 0
    var solIndex = 0
    while (resIndex < results.size && solIndex < solutions.size) {
      val result = results(resIndex)
      val solution = solutions(solIndex)

      if (result.line < solution.line) {
        resIndex += 1
        comparison += Result(Some(result), None, isMatch = false)
      } else if (solution.line < result.line) {
        solIndex += 1
        comparison += Result(None, Some(solution), isMatch = false)
      } else {
        resIndex += 1
        solIndex += 1
        comparison += Result(Some(result), Some(solution), isMatch = result.content == solution.content)
      }
    }

    while (resIndex < results.size) {
      val result = results(resIndex)
      resIndex += 1
      comparison += Result(Some(result), None, isMatch = false)
    }

    while (solIndex < solutions.size) {
      val solution = solutions(solIndex)
      solIndex += 1
      comparison += Result(None, Some(solution), isMatch = false)
    }

    comparison.toList
  }

  private def format(solution: Option[Solution], isMatch: Boolean, colorContent: Boolean, maxLineWidth: Int): String = solution match {
    case Some(Solution(lineNumber, output)) =>
      import formatter._

      val o = if (output.head.isWhitespace || output.last.isWhitespace) s"'$output'" else output
      val outputColor = if (colorContent) getColor(o) else NoColor
      val lineColor = Bold + (if (isMatch) Green else Red)
      val padded = lineNumber.toString.padTo(maxLineWidth, ' ')
      lineColor(padded) + ": " + outputColor(o)
    case None                               => ""
  }

  private def getColor(s: String): Colors.Color = {
    val colors = formatter.FGColors
    colors(math.abs(s.hashCode) % colors.length)
  }

  private def fail(reason: String, extraBoxes: String*): Nothing = {
    throw TestFailedException(reason, extraBoxes.toList)
  }
}
