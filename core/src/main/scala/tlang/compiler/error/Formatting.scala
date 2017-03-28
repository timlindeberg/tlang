package tlang.compiler.error

import java.io.File

import tlang.compiler.Main
import tlang.compiler.error.Boxes.{Box, Simple}
import tlang.repl.{ASCIISpinner, BrailSpinner, Spinner}
import tlang.utils.Extensions._
import tlang.utils.{Colors, Enumeration}

/**
  * Created by Tim Lindeberg on 1/29/2017.
  */

case class Formatting(box: Box, lineWidth: Int, colors: Colors, trim: Boolean = true) {

  import box._
  import colors._

  private val Indent     = 2
  private val Width: Int = lineWidth - (2 * Indent)

  val wordWrapper       = new AnsiWordWrapper
  val syntaxHighlighter = SyntaxHighlighter(colors)

  def spinner: Spinner = if (colors.isActive) BrailSpinner() else ASCIISpinner()

  def top: String = trimRight(┌ + ─ * (lineWidth - Indent) + ┐) + "\n"
  def bottom: String = trimRight(└ + ─ * (lineWidth - Indent) + ┘) + "\n"
  def divider: String = trimRight(├ + ─ * (lineWidth - Indent) + ┤) + "\n"

  def seperator(left: String, bridge: String, right: String, bridgeAt: Int): String = {
    val rest = ─ * (lineWidth - bridgeAt - (2 * Indent + 1))
    val overNumbers = ─ * (bridgeAt + Indent)
    val line = left + overNumbers + bridge + rest + right
    trimRight(line) + "\n"
  }

  def rightAlign(text: String, fill: Char = ' '): String =
    wordWrapper(text, Width).map { line =>
      ("" + fill) * (Width - line.charCount) + line
    }.mkString("\n")

  def center(text: String, fill: Char = ' '): String =
    wordWrapper(text, Width).map { line =>
      val x = Width - line.charCount
      val space = ("" + fill) * (x / 2)
      val left = space
      val right = if (x % 2 == 0) space else space + fill
      left + line + right
    }.mkString("\n")

  def makeHeader(text: String): String = {
    val lines = makeLines(center(text)).mkString
    top + lines
  }

  def makeLines(lines: String, w: Int = Width): String =
    wordWrapper(lines, w).map(makeLine(_, w)).mkString


  def makeLine(line: String, w: Int = Width): String = {
    val whitespaces = " " * (w - line.charCount)
    val l = │ + " " + line + whitespaces + " " + │
    trimRight(l) + "\n"
  }

  def makeBlock(block: String): String = {
    val sb = new StringBuilder
    sb ++= divider
    sb ++= makeLines(block)
    sb.toString()
  }

  def makeBlockWithColumn(block: Traversable[(String, String)], endOfBlock: Boolean): String = {
    val sb = new StringBuilder

    val columnVars = block.unzip._1
    val maxColumnWidth = columnVars.map(_.charCount).max
    val w = Width - maxColumnWidth - 3

    sb ++= seperator(├, ┬, ┤, maxColumnWidth)

    sb ++= block
      .flatMap { case (col, line) =>
        val lines = wordWrapper(line, w)
        val columns = col :: List.fill(lines.size - 1)("")
        columns zip lines
      }
      .map { case (col, line) =>
        val columnWidth = col.charCount
        val whiteSpaces = " " * (maxColumnWidth - columnWidth)
        │ + " " + col + whiteSpaces + " " + makeLine(line, w)
      }.mkString
    sb ++= (if (endOfBlock) seperator(└, ┴, ┘, maxColumnWidth) else seperator(├, ┴, ┤, maxColumnWidth))
    sb.toString.print
  }

  def makeBoxWithColumn(header: String, block: Traversable[(String, String)], endOfBlock: Boolean = true): String = {
    val sb = new StringBuilder
    sb ++= makeHeader(header)
    sb ++= makeBlockWithColumn(block, endOfBlock)
    sb.toString
  }

  def makeBox(header: String, blocks: Traversable[String]): String = {
    val sb = new StringBuilder
    sb ++= makeHeader(header)
    blocks foreach { sb ++= makeBlock(_) }
    sb ++= bottom
    sb.toString
  }

  def formatFileName(file: Option[File]): String = {
    file match {
      case Some(f) => formatFileName(f)
      case None    => Bold(Magenta("No file"))
    }
  }

  def formatFileName(file: File): String = formatFileName(file.getName)

  def formatFileName(name: String): String = Bold(Magenta(name) + Main.FileEnding)


  def makeList(items: Traversable[String], indent: String = "  "): String = {
    val listSign = if (colors.isActive) "•" else "*"
    items.map(item => s"$indent$listSign $item").mkString("\n")
  }

  private def trimRight(s: String) = if (trim) s.rightTrimWhiteSpaces else s
}

object SimpleFormatting extends Formatting(Simple, 80, Colors(isActive = false))

object Boxes {

  val DefaultBox: Box = Light

  sealed abstract class Box(val chars: String) extends Product with Serializable {
    val ─ : String = chars(0).toString
    val │ : String = chars(1).toString
    val ┌ : String = chars(2).toString
    val ┐ : String = chars(3).toString
    val ┘ : String = chars(4).toString
    val └ : String = chars(5).toString
    val ┬ : String = chars(6).toString
    val ┴ : String = chars(7).toString
    val ├ : String = chars(8).toString
    val ┤ : String = chars(9).toString
    val ┼ : String = chars(10).toString

    // Drop right to remove $ at end of object class name
    val name: String = getClass.getSimpleName.dropRight(1)
  }


  // @formatter:off
  case object Simple             extends Box("-|    --||-")
  case object NoLines            extends Box("           ")
  case object Double             extends Box("═║╔╗╝╚╦╩╠╣╬")
  case object Light              extends Box("─│┌┐┘└┬┴├┤┼")
  case object Heavy              extends Box("━┃┏┓┛┗┳┻┣┫╋")
  case object DoubleDashLight    extends Box("╌╎┌┐┘└┬┴├┤┼")
  case object DoubleDashHeavy    extends Box("╍╏┏┓┛┗┳┻┣┫╋")
  case object TripleDashLight    extends Box("┄┆┌┐┘└┬┴├┤┼")
  case object TripleDashHeavy    extends Box("┅┇┏┓┛┗┳┻┣┫╋")
  case object QuadrupleDashLight extends Box("┈┊┌┐┘└┬┴├┤┼")
  case object QuadrupleDashHeavy extends Box("┉┋┏┓┛┗┳┻┣┫╋")
  // @formatter:on

  lazy val All: Set[Box] = Enumeration.instancesOf[Box]
}
