package tcompiler.error

import tcompiler.utils.Extensions._

import scala.annotation.tailrec

/**
  * Created by Tim Lindeberg on 1/29/2017.
  */
class AnsiWordWrapper {

  private val EarlyBreakChars: String = """\/.:;-_()"""


  def apply(texts: List[String], maxWidth: Int): List[String] = texts.flatMap(apply(_, maxWidth))

  def apply(text: String, maxWidth: Int): List[String] = {

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
          wordWrap(rest, currentWord + s"\u001b[$a${b}m", currentLine, lines)
        case '\u001b' :: '[' :: a :: 'm' :: rest      =>
          wordWrap(rest, currentWord + s"\u001b[${a}m", currentLine, lines)
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
    }
  }

  private def wrapAnsi(lines: List[String]): List[String] = {

    @tailrec def getAnsi(chars: List[Char], ansi: List[String]): List[String] = chars match {
      case '\u001b' :: '[' :: '0' :: 'm' :: rest    => getAnsi(rest, Nil)
      case '\u001b' :: '[' :: a :: b :: 'm' :: rest => getAnsi(rest, s"\u001b[$a${
        b
      }m" :: ansi)
      case '\u001b' :: '[' :: a :: 'm' :: rest      => getAnsi(rest, s"\u001b[${
        a
      }m" :: ansi)
      case _ :: rest                                => getAnsi(rest, ansi)
      case Nil                                      => ansi
    }

    val seq = lines.toIndexedSeq
    var ansi: List[String] = Nil
    seq.indices.map {
      i =>
        var sb = new StringBuilder
        if (ansi.nonEmpty)
          sb ++= ansi.mkString

        sb ++= seq(i)
        ansi = getAnsi(seq(i).toList, ansi)
        if (ansi.nonEmpty)
          sb ++= Console.RESET
        sb.toString
    }.toList
  }

}
