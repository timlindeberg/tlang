package tlang.testutils

import org.scalatest.matchers.{MatchResult, Matcher}
import tlang.utils.Extensions._

// When comparing Strings with ansi colors the parser for IntelliJ:s test runner
// breaks. Comparing lists of characters works a lot better and has the added benefit
// of letting us see exact differences in ansi formatting.
object AnsiMatchers extends AnsiMatchers
trait AnsiMatchers {

  private def format(found: String, expected: String): String =
    s"""
       |${ formatLine(expected, "Expected") }
       |${ formatLine(found, "  Actual") }""".stripMargin


  private def formatLine(line: String, name: String, seperator: String = ": "): String = {
    val lines = line.split("\n").map(l => if (l.isEmpty) "<empty>" else l.ansiDebugString)
    val prefix = s"$name$seperator"
    val first = prefix + lines.head
    if (lines.length == 1)
      return first

    val indent = " " * prefix.length
    first + "\n" + lines.tail.map(indent + _).mkString("\n")
  }


  class AnsiStringMatcher(expected: String) extends Matcher[String] {

    def apply(found: String): MatchResult = {
      MatchResult(
        found == expected,
        s"The strings did not match:${ format(found, expected) }",
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
          formatLine(actual, s"  Actual $num: $c  ", " ") + "\n" +
            formatLine(expected, s"Expected $num: $c  ", " ")
        }
        .mkString("\n")
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
        .map { case ((actual, expected), i) => (false, s"String number ${ i + 1 } did not match:${ format(actual, expected) }") }
        .getOrElse((true, s""))
      MatchResult(matches, failMessage, Matched)
    }

  }

  def matchWithAnsi(expectedString: String) = new AnsiStringMatcher(expectedString)
  def allMatchWithAnsi(expectedString: String, moreStrings: String*) = new MultipleAnsiStringMatcher(expectedString :: moreStrings.toList)


}
