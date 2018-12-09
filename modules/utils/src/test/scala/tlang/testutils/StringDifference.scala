package tlang
package testutils


object StringDifference {

  def apply(f: String, e: String): String = {
    val found = if (f == null) "null" else f
    val expected = if (e == null) "null" else e

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
    sbFound.toString + NL + sbExpected.toString + NL + sbDifference.toString
  }

  private def replaceChar(c: Char): String = c match {
    case '\u001b' => "▯"
    case '\n'     => "\\n"
    case '\r'     => "\\r"
    case '\t'     => "\\t"
    case c        => s"$c"
  }

}
