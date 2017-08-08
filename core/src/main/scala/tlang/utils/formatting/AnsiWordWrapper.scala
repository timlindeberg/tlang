package tlang.utils.formatting

import tlang.utils.Extensions._

import scala.annotation.tailrec
import scala.collection.mutable

case class AnsiWordWrapper() {

  private val EarlyBreakChars: String = """\/.:;-_()"""


  def apply(texts: List[String], maxWidth: Int): List[String] = texts.flatMap(apply(_, maxWidth))

  def apply(text: String, maxWidth: Int, maxLines: Int = -1): List[String] = {

    @tailrec def whiteSpaces(chars: List[Char], s: StringBuilder = new StringBuilder): (String, List[Char]) = chars match {
      case c :: rest if isWhiteSpace(c) => whiteSpaces(rest, s + c)
      case rest                         => (s.toString, rest)
    }


    @tailrec def wordWrap(chars: List[Char], currentWord: String, currentLine: String, lines: List[String]): List[String] = {
      def currentLength: Int = currentLine.charCount + currentWord.charCount

      def newLine(): List[String] = {
        if (currentLength > maxWidth) {
          val (word, wrappedLines) = wrap(currentWord, currentLine, maxWidth)
          word :: wrappedLines ::: lines
        } else {
          currentLine + currentWord :: lines
        }
      }

      chars match {
        case '\u001b' :: '[' :: '0' :: 'm' :: rest    =>
          wordWrap(rest, currentWord + Console.RESET, currentLine, lines)
        case '\u001b' :: '[' :: a :: b :: 'm' :: rest =>
          wordWrap(rest, currentWord + s"\u001b[$a${ b }m", currentLine, lines)
        case '\u001b' :: '[' :: a :: 'm' :: rest      =>
          wordWrap(rest, currentWord + s"\u001b[${ a }m", currentLine, lines)
        case '\r' :: '\n' :: rest                     =>
          wordWrap(rest, "", "", newLine())
        case '\n' :: rest                             =>
          wordWrap(rest, "", "", newLine())
        case c :: _ if isWhiteSpace(c)                =>
          val (spaces, rest) = whiteSpaces(chars)
          val h = currentLength
          if (currentLength > maxWidth) {
            val (word, wrappedLines) = wrap(currentWord, currentLine, maxWidth)
            wordWrap(rest, "", word + spaces, wrappedLines ::: lines)
          } else {
            wordWrap(rest, spaces, currentLine + currentWord, lines)
          }
        case c :: rest                                =>
          wordWrap(rest, currentWord + c, currentLine, lines)
        case Nil                                      =>
          val res = if (currentLength > maxWidth) {
            val (word, wrappedLines) = wrap(currentWord, currentLine, maxWidth)
            word :: wrappedLines ::: lines
          } else {
            currentLine + currentWord :: lines
          }
          res.reverse
      }
    }

    val wrapped = wordWrap(text.toList, "", "", Nil)
    wrapAnsi(wrapped)
  }

  private def isWhiteSpace(c: Char) = c in " \t"

  private def trimWhiteSpace(s: String) = s.dropWhile(isWhiteSpace)

  private def wrap(currentWord: String, currentLine: String, maxWidth: Int): (String, List[String]) = {
    val trimmed = trimWhiteSpace(currentWord)
    if (trimmed.charCount <= maxWidth)
      return (trimmed, List(currentLine))


    var word = trimmed
    var line = currentLine
    var lines: List[String] = Nil

    do {
      if (line == "") {
        val breakpoint = findBreakpoint(word, maxWidth)

        val (w1, w2) = word.splitAt(breakpoint)

        word = w2
        lines ::= w1
      } else {
        val breakpoint = findBreakpoint(word, maxWidth - line.charCount - 1)

        val (w1, w2) = word.splitAt(breakpoint)

        word = w2
        lines ::= line + w1
        line = ""
      }
    } while (word.charCount > maxWidth)

    (word, lines)
  }


  private def findBreakpoint(word: String, width: Int): Int = {
    val chars = word.toList
    val breakPoints = findEarlyBreakPoints(chars, 0, width, Nil)
    breakPoints.headOption.getOrElse(findBreakPoint(chars, 0, width))
  }

  @tailrec private def findEarlyBreakPoints(chars: List[Char], index: Int, width: Int, breakPoints: List[Int]): List[Int] = {
    if (width <= 0)
      return breakPoints

    chars match {
      case '\u001b' :: '[' :: _ :: _ :: 'm' :: rest => findEarlyBreakPoints(rest, index + 5, width, breakPoints)
      case '\u001b' :: '[' :: _ :: 'm' :: rest      => findEarlyBreakPoints(rest, index + 4, width, breakPoints)
      case c :: rest if c in EarlyBreakChars        => findEarlyBreakPoints(rest, index + 1, width - 1, index + 1 :: breakPoints)
      case _ :: rest                                => findEarlyBreakPoints(rest, index + 1, width - 1, breakPoints)
    }
  }

  @tailrec private def findBreakPoint(chars: List[Char], index: Int, width: Int): Int = {
    if (width <= 0)
      return index

    chars match {
      case '\u001b' :: '[' :: _ :: _ :: 'm' :: rest => findBreakPoint(rest, index + 5, width)
      case '\u001b' :: '[' :: _ :: 'm' :: rest      => findBreakPoint(rest, index + 4, width)
      case _ :: rest                                => findBreakPoint(rest, index + 1, width - 1)
      case _                                        => ???
    }
  }

  private def wrapAnsi(lines: List[String]): List[String] = {


    var currentColor: String = ""
    var currentBGColor: String = ""
    var SGRs = mutable.Set[String]()
    var ansi = ""

    def updateFrom(line: String, index: Int): Int = {
      val endOfAnsi = line.indexOf('m', index + 1)
      val ansiValues = line.subSequence(index + 2, endOfAnsi).toString

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
      ansi = currentColor + currentBGColor + SGRs.mkString
      endOfAnsi
    }

    def updateAnsi(line: String): Unit = {
      var i = 0
      while (i < line.length) {
        line(i) match {
          case '\u001b' if line(i + 1) == '[' =>
            val endOfAnsi = updateFrom(line, i)
            ansi = currentColor + currentBGColor + SGRs.mkString
            i = endOfAnsi
          case _                              =>
        }
        i += 1
      }
    }

    lines.map { line =>
      var sb = new StringBuilder
      if (line.startsWith("\u001b[")) {
        // Make sure we don't repeat the previous ansi if it is immediately updated
        val endOfAnsi = updateFrom(line, 0)
        sb ++= ansi
        sb ++= line.substring(endOfAnsi + 1)
      } else {
        sb ++= ansi
        sb ++= line
      }
      updateAnsi(line)
      if (ansi.nonEmpty)
        sb ++= Console.RESET
      sb.toString
    }
  }

}
