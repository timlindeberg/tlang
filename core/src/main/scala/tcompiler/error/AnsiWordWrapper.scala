package tcompiler.error

import tcompiler.utils.Colorizer
import tcompiler.utils.Extensions._

import scala.annotation.tailrec

/**
  * Created by Tim Lindeberg on 1/29/2017.
  */
class AnsiWordWrapper(colorizer: Colorizer) {

  import colorizer._

  def apply(text: String, maxWidth: Int): List[String] = {

    @tailrec def wordWrap(chars: List[Char], currentWord: String, currentLine: String, lines: List[String]): List[String] = {
      chars match {
        case '\u001b' :: '[' :: '0' :: 'm' :: rest    =>
          wordWrap(rest, currentWord + Reset, currentLine, lines)
        case '\u001b' :: '[' :: a :: b :: 'm' :: rest =>
          wordWrap(rest, currentWord + s"\u001b[$a${b}m", currentLine, lines)
        case '\u001b' :: '[' :: a :: 'm' :: rest      =>
          wordWrap(rest, currentWord + s"\u001b[${a}m", currentLine, lines)
        case c :: rest if c.isWhitespace              =>
          val currentLength = currentLine.charCount + currentWord.charCount + 1
          if (currentLength > maxWidth) {
            val (word, wrappedLines) = wrap(currentWord, currentLine, maxWidth)
            wordWrap(rest, "", word, wrappedLines ::: lines)
          } else {
            val line = if (currentLine == "") currentWord else currentLine + " " + currentWord
            wordWrap(rest, "", line, lines)
          }
        case c :: rest                                =>
          wordWrap(rest, currentWord + c, currentLine, lines)
        case Nil                                      =>
          val currentLength = currentLine.charCount + currentWord.charCount + 1
          val res = if (currentLength > maxWidth) {
            val (word, wrappedLines) = wrap(currentWord, currentLine, maxWidth)
            word :: wrappedLines ::: lines
          } else {
            val line = if (currentLine == "") currentWord else currentLine + " " + currentWord
            line :: lines
          }
          res.reverse
      }
    }

    val wrapped = wordWrap(text.toList, "", "", Nil)
    wrapAnsi(wrapped)
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
      case c :: rest if c in "\\/.:;-_()"           => findEarlyBreakPoints(rest, index + 1, width - 1, index + 1 :: breakPoints)
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
    }
  }


  private def wrap(currentWord: String, currentLine: String, maxWidth: Int): (String, List[String]) = {
    if (currentWord.charCount <= maxWidth)
      return (currentWord, List(currentLine))


    var word = currentWord
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
        lines ::= line + " " + w1
        line = ""
      }
    } while (word.charCount > maxWidth)

    (word, lines)
  }

  private def wrapAnsi(lines: List[String]): List[String] = {

    @tailrec def getAnsi(chars: List[Char], ansi: List[String]): List[String] = chars match {
      case '\u001b' :: '[' :: '0' :: 'm' :: rest    => getAnsi(rest, Nil)
      case '\u001b' :: '[' :: a :: b :: 'm' :: rest => getAnsi(rest, s"\u001b[$a${b}m" :: ansi)
      case '\u001b' :: '[' :: a :: 'm' :: rest      => getAnsi(rest, s"\u001b[${a}m" :: ansi)
      case _ :: rest                                => getAnsi(rest, ansi)
      case Nil                                      => ansi
    }

    val seq = lines.toIndexedSeq
    var ansi: List[String] = Nil
    seq.indices.map { i =>
      var sb = new StringBuilder
      if (ansi.nonEmpty)
        sb ++= ansi.mkString

      sb ++= seq(i)
      ansi = getAnsi(seq(i).toList, ansi)
      if (ansi.nonEmpty)
        sb ++= Reset
      sb.toString
    }.toList
  }

}
