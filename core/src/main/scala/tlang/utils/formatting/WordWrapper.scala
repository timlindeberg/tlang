package tlang.utils.formatting

import tlang.utils.Extensions._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

case class WordWrapper() {


  val BreakChars  = "-/._\\;:()"
  val WhiteSpaces = " \t"


  def apply(text: String, maxWidth: Int): List[String] = {

    var currentColor: String = ""
    var currentBGColor: String = ""
    var SGRs = mutable.Set[String]()
    var currentAnsi = ""

    var hasWrapped = false

    var spaceStart = 0
    var spaceLen = 0
    var lineLen = 0
    var wordStart = 0
    var wordLen = 0
    val lines: ListBuffer[String] = ListBuffer()
    val currentLine = new StringBuilder(2 * maxWidth)

    def addLine(): Unit = {
      hasWrapped = true
      lines += currentLine.toString
      currentLine.clear()
      lineLen = 0
      spaceLen = 0
    }

    def addWordToLine(word: String, len: Int) = {
      currentLine ++= word
      lineLen += len
      wordLen = 0
    }

    def addSpacesToLine(len: Int): Int = {
      // If we're at the start of a line and already have wrapped we don't
      // want to add any indentation
      if (lineLen == 0 && hasWrapped)
        return 0

      // If we can make it fit by removing some spaces
      // we do so, otherwise use all the spaces
      val numSpaces = Math.min(spaceLen, maxWidth - (lineLen + len))
      currentLine.appendTimes(' ', numSpaces)
      numSpaces
    }


    def updateAnsi(text: String, index: Int): Int = {
      val endOfAnsi = text.indexOf('m', index + 1)
      val ansiValues = text.subSequence(index + 2, endOfAnsi).toString

      ansiValues.split(":").map(_.toList).foreach {
        case '0' :: Nil                           =>
          currentColor = ""
          currentBGColor = ""
          SGRs.clear()
        case ('1' | '4') :: Nil                   => SGRs += s"\u001b[${ ansiValues }m"
        case '3' :: c :: Nil if c in ('1' to '7') => currentColor = s"\u001b[${ ansiValues }m"
        case '4' :: c :: Nil if c in ('1' to '7') => currentBGColor = s"\u001b[${ ansiValues }m"
        case _                                    =>
      }
      // Guarantees the same order each time, for instance Bold will appear before Underline since [1m < [4m
      currentAnsi = SGRs.toList.sorted.mkString + currentColor + currentBGColor
      endOfAnsi
    }

    def findBreakpoint(word: String, widthAvailable: Int): (Int, Int) = {
      var i = 0
      var breakPoint = -1
      var len = 0
      while (i < word.length) {
        text(i) match {
          case '\u001b' if text(i + 1) == '[' =>
            i = text.indexOf('m', i + 1)
          case c                              =>
            if (c in BreakChars)
              breakPoint = i
            len += 1
            if (len == widthAvailable) {
              if (breakPoint == -1) breakPoint = widthAvailable
              return (breakPoint, len)
            }
        }
        i += 1
      }
      (breakPoint, len)
    }

    var i = 0

    def endOfWord(): Unit = {
      if (wordLen == 0)
        return

      // The word including potential ansi formatting
      var word = text.substring(wordStart, i)
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
        val (w, nextWord) = word.splitAt(breakPoint)
        if (len <= spaceLeft) {
          val numSpaces = addSpacesToLine(len)
          addWordToLine(w, len + numSpaces)
          addLine()
        } else {
          addLine()
          addWordToLine(w, len)
        }
        wordLen = restLen
        word = nextWord
      }
      if (wordLen > 0)
        addWordToLine(word, wordLen)
      wordLen = 0
    }

    while (i < text.length) {
      val c = text(i)
      c match {
        case '\u001b' if text(i + 1) == '[' =>
          // Update the current ansi colors and skip past the ansi characters
          // They are not added towards the current length
          i = updateAnsi(text, i)
        case '\r' if text(i + 1) == '\n'    =>
          addLine()
          i += 1
          hasWrapped = false
        case '\n'                           =>
          addLine()
          hasWrapped = false
        case c if c in WhiteSpaces          =>
          endOfWord()
          spaceLen += 1
          while (text(i + 1) in WhiteSpaces) {
            i += 1
            spaceLen += 1
          }
          wordStart = i + 1
        case _                              =>
          wordLen += 1
      }
      i += 1
    }
    endOfWord()
    if (currentLine.nonEmpty)
      addLine()
    lines.toList
  }


}
