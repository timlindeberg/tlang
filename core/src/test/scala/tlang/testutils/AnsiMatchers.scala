package tlang.testutils

import org.scalatest.matchers.{MatchResult, Matcher}

// When comparing Strings with ansi colors the parser for IntelliJ:s test runner
// breaks. Comparing lists of characters works a lot better and has the added benefit
// of letting us see exact differences in ansi formatting.
object AnsiMatchers extends AnsiMatchers
trait AnsiMatchers {

  private def format(found: String, expected: String): String =
    s"""
       |${ formatLines(found, "   Found") }
       |${ formatLines(expected, "Expected") }""".stripMargin

  private def formatLines(s: String, name: String): String = {
    val lines = s.split("\n")
      .map { line =>
        line.toList
          .map {
            case '\u001b' => '�'
            case c        => c
          }
          .map("'" + _ + "'")
          .mkString(" ")
      }

    val prefix = s"$name: "
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

    def apply(foundStrings: Iterable[String]): MatchResult = {
      val numExpected = expectedStrings.size
      val numFound = foundStrings.size
      if (numExpected != numFound)
        return MatchResult(matches = false, s"Expected '$numExpected' strings, got '$numFound'", Matched)

      val (matches, onFailure) = foundStrings
        .zip(expectedStrings)
        .zipWithIndex
        .find { case ((found, expected), _) => found != expected }
        .map { case ((found, expected), i) => (false, s"String number ${ i + 1 } did not match:${ format(found, expected) }") }
        .getOrElse((true, s""))
      MatchResult(matches, onFailure, Matched)
    }

  }

  def matchWithAnsi(expectedString: String) = new AnsiStringMatcher(expectedString)
  def allMatchWithAnsi(expectedString: String, moreStrings: String*) = new MultipleAnsiStringMatcher(expectedString :: moreStrings.toList)


}
