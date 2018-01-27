package tlang.formatting.textformatters

import tlang.formatting.Colors
import tlang.formatting.Colors.{Color, NoColor, Reset, extractColorFrom}
import tlang.utils.Extensions._

import scala.collection.mutable.ListBuffer

case class WordWrapper(tabSize: Int = 2, wrapAnsiColors: Boolean = true) {


  val BreakCharacters = "-/.,_\\;:()[]{}"
  val WhiteSpaces     = " \t"

  def apply(text: String, maxWidth: Int): List[String] = {
    val lineBuffer: ListBuffer[String] = ListBuffer()

    text.split("\r?\n", -1) foreach { wrap(_, maxWidth, lineBuffer) }

    val lines = lineBuffer.toList
    if (wrapAnsiColors) wrapAnsiFormatting(lineBuffer.toList) else lines
  }

  def wrapAnsiFormatting(lines: List[String]): List[String] = {
    var color: Color = Colors.NoColor

    def updateAnsi(line: String): Unit = {
      var i = 0
      while (i < line.length) {
        line(i) match {
          case '\u001b' if line(i + 1) == '[' =>
            val (newColor, endOfColor) = extractColorFrom(line, i, color)
            color = newColor
            i = endOfColor
          case _                              =>
        }
        i += 1
      }
    }

    lines map { line =>
      var sb = new StringBuilder
      if (line.startsWith("\u001b[")) {
        // Make sure we don't repeat the previous ansi if it is immediately updated
        val (newColor, endOfColor) = extractColorFrom(line, 0, color)
        color = newColor
        sb ++= color
        sb ++= line.substring(endOfColor)
      } else {
        sb ++= color
        sb ++= line
      }
      updateAnsi(line)
      if (color != NoColor && !line.endsWith(Reset))
        sb ++= Reset
      sb.toString
    }
  }

  private def wrap(line: String, maxWidth: Int, lines: ListBuffer[String]): Unit = {
    val visibleChars = line.visibleCharacters + (line.count(_ == '\t') * (tabSize - 1))
    if (line.visibleCharacters == 0) {
      lines += ""
      return
    }
    if (visibleChars <= maxWidth) {
      lines += line.replaceAll("\t", " " * tabSize)
      return
    }

    var hasWrapped = false

    var (spaceLen, lineLen, wordLen) = (0, 0, 0)
    var wordStart = 0
    val currentLine = new StringBuilder(2 * maxWidth) // to make room for one line and hopefully some ANSI escape codes

    def addLine(): Unit = {
      if (lineLen == 0)
        return

      lines += currentLine.toString
      currentLine.clear()

      hasWrapped = true
      lineLen = 0
    }

    def addWordToLine(word: String, len: Int): Unit = {
      currentLine ++= word
      lineLen += len
      wordLen = 0
    }

    def addSpacesToLine(len: Int): Int = {
      // If we're at the start of a line and already have wrapped we don't
      // want to add any spaces. We should only preserve indentation for the first
      // line.
      if (lineLen == 0 && hasWrapped)
        return 0

      // If we can make it fit by removing some spaces
      // we do so, otherwise use all the spaces
      val numSpaces = Math.min(spaceLen, maxWidth - (lineLen + len))
      currentLine.appendTimes(' ', numSpaces)
      numSpaces
    }

    // When reaching the end of a word (or the end of input) we need to determine
    // whether we add the word to the current line, put it on the next line or
    // split the word up
    def endOfWord(endIndex: Int): Unit = {
      if (wordLen == 0) {
        if (endIndex > 0 && currentLine.isEmpty) {
          // We're at the start of a line, the word is empty but the
          // the end index is larger than 0. That must mean that we
          // had ansi characters at the start of the word
          currentLine ++= line.substring(0, endIndex)
        }
        return
      }

      // The word including potential ansi formatting
      var word = line.substring(wordStart, endIndex)
      // The word fits on the line if we use one space
      if (lineLen + 1 + wordLen <= maxWidth) {
        val numSpaces = addSpacesToLine(wordLen)
        addWordToLine(word, wordLen + numSpaces)
        return
      }

      // The word doesn't fit on the current line but is smaller than one line
      // so we add it to the next line
      if (lineLen + 1 + wordLen > maxWidth && wordLen <= maxWidth) {
        addLine()
        // Skip spaces when wrapping the line since we never want to start
        // a wrapped line with spaces
        addWordToLine(word, wordLen)
        return
      }

      // If we got here the word is larger than one line,
      // we have to split it up
      while (wordLen > maxWidth) {
        // If there is content on the line we need an additional space
        val spaceLeft = if (lineLen == 0) maxWidth else maxWidth - lineLen - 1
        val (breakPoint, len) = findBreakpoint(word, spaceLeft)
        val restLen = wordLen - len
        val (w, nextPart) = word.splitAt(breakPoint)
        if (breakPoint == -1) {
          // If we couldn't find a place to split the word we use the line as it is
          addLine()
        } else if (len <= spaceLeft) {
          // The split word fits on the given line, add the split and then the line
          val numSpaces = addSpacesToLine(len)
          addWordToLine(w, len + numSpaces)
          addLine()
        } else {
          // The split word does not fit on the line, add it first and add the split
          // to the next line
          addLine()
          addWordToLine(w, len)
        }
        wordLen = restLen
        word = nextPart
      }
      if (wordLen > 0)
        addWordToLine(word, wordLen)
      wordLen = 0
    }

    var i = 0
    while (i < line.length) {
      val c = line(i)
      c match {
        case '\u001b' if line(i + 1) == '[' =>
          // Skip past ANSI characters since they are not added towards the word length
          i = line.indexOf('m', i + 1)
        case c if c in WhiteSpaces          =>
          endOfWord(i)
          spaceLen = if (c == '\t') tabSize else 1
          while (i + 1 < line.length && (line(i + 1) in WhiteSpaces)) {
            spaceLen += (if (line(i + 1) == '\t') tabSize else 1)
            i += 1
          }
          wordStart = i + 1
        case _                              =>
          wordLen += 1
      }
      i += 1
    }

    endOfWord(i)
    if (lineLen > 0)
      addLine()

    if (!hasWrapped)
      lines += " " * spaceLen
  }

  // Locates a place to break the word up. If there exists any special characters in the string,
  // or if there exists a place where the string goes from lowercase to uppercase it will break
  // at the last such character otherwise it will break at the last possible character for the
  // given width.
  // Returns the break point index and the number of visible characters up to that point
  private def findBreakpoint(word: String, widthAvailable: Int): (Int, Int) = {
    if (widthAvailable <= 0)
      return (-1, 0)

    var i = 0
    var breakPoint = -1
    var breakLen = 0
    var len = 0
    while (i < word.length) {
      word(i) match {
        case '\u001b' if word(i + 1) == '[' =>
          // Don't count the length of ANSI characters
          i = word.indexOf('m', i + 1)
        case c                              =>
          len += 1

          val isCamelCase = i + 1 < word.length && c.isLower && word(i + 1).isUpper
          if ((c in BreakCharacters) || isCamelCase) {
            breakPoint = i + 1
            breakLen = len
          }
          if (len == widthAvailable)
            return if (breakPoint == -1) (i + 1, len) else (breakPoint, breakLen)
      }
      i += 1
    }
    (breakPoint, 0)
  }

}
