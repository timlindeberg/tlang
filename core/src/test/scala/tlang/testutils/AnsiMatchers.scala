package tlang.testutils

import org.scalatest.matchers.{MatchResult, Matcher}
import tlang.utils.Extensions._

// When comparing Strings with ansi colors the parser for IntelliJ:s test runner
// breaks. Comparing lists of characters works a lot better and has the added benefit
// of letting us see exact differences in ansi formatting.
object AnsiMatchers extends AnsiMatchers
trait AnsiMatchers {

  private def format(found: String, expected: String): String = {
    val sbFound = new StringBuilder("  Actual:")
    val sbExpected = new StringBuilder("Expected:")
    val sbDifference = new StringBuilder("         ")
    var i = 0
    while (i < found.length || i < expected.length) {
      var f = if (i < found.length) replaceChar(found(i)) else ""
      var e = if (i < expected.length) replaceChar(expected(i)) else ""

      // Since \r and \n is two characters we need to add a space to keep alignment
      (f.length, e.length) match {
        case (2, 1) => e += " "
        case (1, 2) => f += " "
        case _      =>
      }

      if (f.nonEmpty) sbFound ++= " '" + f + "'"
      if (e.nonEmpty) sbExpected ++= " '" + e + "'"

      val diffChar = if (f != e) "‾" else " "
      sbDifference ++= " " + diffChar * (2 + math.max(f.length, e.length))
      i += 1
    }
    sbFound.toString + System.lineSeparator + sbExpected.toString + System.lineSeparator + sbDifference.toString
  }

  private def replaceChar(c: Char): String = c match {
    case '\u001b' => "▯"
    case '\n'     => "\\n"
    case '\r'     => "\\r"
    case '\t'     => "\\t"
    case c        => s"$c"
  }

  private def formatLine(line: String, name: String, seperator: String = ": "): String = {
    val l = if (line.isEmpty) "<empty" else line.ansiDebugString
    s"$name$seperator$l"
  }

  private def differenceIndicator(indent: Int, found: String, expected: String): String = {
    val sb = new StringBuilder(found.length)
    sb ++= " " * indent
    found.zip(expected).foreach { case (a, b) =>
      sb ++= "  "

      if (a == b)
        if (a == '\r' || a == '\n')
          sb ++= "  "
        else
          sb += ' '
      else
        sb += '‾'

      sb += ' '
    }
    sb.toString
  }

  class AnsiStringMatcher(expected: String) extends Matcher[String] {

    def apply(found: String): MatchResult = {
      MatchResult(
        found == expected,
        s"The strings did not match:\n${ format(found, expected) }",
        s"The strings matched."
      )
    }

  }

  class MultipleAnsiStringMatcher(expectedStrings: Iterable[String]) extends Matcher[Iterable[String]] {

    val Matched = "All strings matched"

    private def errMessage(actual: Iterable[String], expected: Iterable[String]) = {
      s"""|Expected '${ expected.size }' strings, found '${ actual.size }':
          |${ formatResult(actual, expected) }
           """.stripMargin
    }

    private def formatResult(actual: Iterable[String], expected: Iterable[String]) = {
      actual
        .zipAll(expected, "-", "-")
        .zipWithIndex
        .map { case ((actual, expected), i) =>
          val c = if (actual == expected) "✓" else "×  "
          val num = i + 1
          formatLine(actual, s"  Actual $num: $c  ", " ") + System.lineSeparator +
            formatLine(expected, s"Expected $num: $c  ", " ")
        }
        .mkString(System.lineSeparator)
    }

    def apply(actualStrings: Iterable[String]): MatchResult = {
      val numExpected = expectedStrings.size
      val numFound = actualStrings.size
      if (numExpected != numFound) {
        return MatchResult(matches = false, errMessage(actualStrings, expectedStrings), Matched)
      }

      val (matches, failMessage) = actualStrings
        .zip(expectedStrings)
        .zipWithIndex
        .find { case ((actual, expected), _) => actual != expected }
        .map { case ((actual, expected), i) => (false, s"String number ${ i + 1 } did not match:\n${ format(actual, expected) }") }
        .getOrElse((true, s""))
      MatchResult(matches, failMessage, Matched)
    }

  }

  def matchWithAnsi(expectedString: String) = new AnsiStringMatcher(expectedString)
  def allMatchWithAnsi(expectedString: String, moreStrings: String*) = new MultipleAnsiStringMatcher(expectedString :: moreStrings.toList)


}
