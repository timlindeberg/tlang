package tlang.compiler.error

import java.io.File

import tlang.compiler.Main
import tlang.compiler.ast.PrettyPrinter
import tlang.compiler.error.Boxes.{Box, Light, Simple}
import tlang.compiler.options.Flags.LineWidth
import tlang.repl.StackTraceHighlighter
import tlang.utils.Colors.{Color, ColorScheme, DefaultColorScheme}
import tlang.utils.Enumeration
import tlang.utils.Extensions._

import scala.concurrent.duration.FiniteDuration

object FancyFormatting extends Formatting(Light, LineWidth.defaultValue, useColor = true, asciiOnly = false)
object SimpleFormatting extends Formatting(Simple, LineWidth.defaultValue, useColor = false, asciiOnly = true)

case class Formatting(
  box: Box,
  lineWidth: Int,
  colorScheme: ColorScheme = DefaultColorScheme,
  useColor: Boolean = true,
  asciiOnly: Boolean = false,
  trim: Boolean = true) {

  import box._

  import Console._

  /*-------------------------------- Colors --------------------------------*/

  val NoColor   = Color("", isActive = false)
  val Reset     = Color(RESET, useColor)
  val Bold      = Color(BOLD, useColor)
  val Underline = Color(UNDERLINED, useColor)

  val Black   = Color(BLACK, useColor)
  val Red     = Color(RED, useColor)
  val Green   = Color(GREEN, useColor)
  val Yellow  = Color(YELLOW, useColor)
  val Blue    = Color(BLUE, useColor)
  val Magenta = Color(MAGENTA, useColor)
  val Cyan    = Color(CYAN, useColor)
  val White   = Color(WHITE, useColor)

  val BlackBG   = Color(BLACK_B, useColor)
  val RedBG     = Color(RED_B, useColor)
  val GreenBG   = Color(GREEN_B, useColor)
  val YellowBG  = Color(YELLOW_B, useColor)
  val BlueBG    = Color(BLUE_B, useColor)
  val MagentaBG = Color(MAGENTA_B, useColor)
  val CyanBG    = Color(CYAN_B, useColor)
  val WhiteBG   = Color(WHITE_B, useColor)

  val AllColors: Array[Color] = Array(Red, Green, White, Yellow, Blue, Reset, Magenta, Cyan)

  /*-------------------------------- Color Scheme --------------------------------*/

  val KeywordColor = Color(colorScheme.Keyword, useColor)
  val VarColor     = Color(colorScheme.Variable, useColor)
  val ClassColor   = Color(colorScheme.Class, useColor)
  val MethodColor  = Color(colorScheme.Method, useColor)
  val StringColor  = Color(colorScheme.String, useColor)
  val NumColor     = Color(colorScheme.Number, useColor)
  val CommentColor = Color(colorScheme.Comment, useColor)
  val SymbolColor  = Color(colorScheme.Symbol, useColor)

  /*-------------------------------- Utilities --------------------------------*/

  val wordWrapper           = AnsiWordWrapper()
  val syntaxHighlighter     = SyntaxHighlighter(this)
  val stackTraceHighlighter = StackTraceHighlighter(this)
  val prettyPrinter         = PrettyPrinter(this)

  val listMarker: String = if (asciiOnly) "*" else "•"

  def spinner: Spinner = if (asciiOnly) ASCIISpinner() else BrailSpinner()

  /*-------------------------------- Box handling --------------------------------*/

  private val Indent     = 2
  private val Width: Int = lineWidth - (2 * Indent)


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
    items.map(item => s"$indent$listMarker $item").mkString("\n")
  }

  private def trimRight(s: String) = if (trim) s.rightTrimWhiteSpaces else s

}


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

  lazy val All: List[Box] = Enumeration.instancesOf[Box]
}

sealed abstract class Spinner(val frameTime: FiniteDuration, images: String*) {

  private var index        = 0
  private var _elapsedTime = FiniteDuration(0, "ms")

  def nextImage: String = {
    _elapsedTime += frameTime
    val image = images(index)
    index = (index + 1) % images.size
    image
  }

  def elapsedTime: FiniteDuration = _elapsedTime
  def reset(): Unit = {
    _elapsedTime = FiniteDuration(0, "ms")
    index = 0
  }
}

case class AwesomeSpinner() extends Spinner(FiniteDuration(100, "ms"),
  "▁▂▃▄▅▆▇█▇▆▅▄▃▂▁",
  "▂▃▄▅▆▇█▇█▇▆▅▄▃▂",
  "▃▄▅▆▇█▇▆▇█▇▆▅▄▃",
  "▄▅▆▇█▇▆▅▆▇█▇▆▅▄",
  "▅▆▇█▇▆▅▄▅▆▇█▇▆▅",
  "▆▇█▇▆▅▄▃▄▅▆▇█▇▆",
  "▇█▇▆▅▄▃▂▃▄▅▆▇█▇",
  "█▇▆▅▄▃▂▁▂▃▄▅▆▇█",
  "▇▆▅▄▃▂▁▂▁▂▃▄▅▆▇",
  "▆▅▄▃▂▁▂▃▂▁▂▃▄▅▆",
  "▅▄▃▂▁▂▃▄▃▂▁▂▃▄▅",
  "▄▃▂▁▂▃▄▅▄▃▂▁▂▃▄",
  "▃▂▁▂▃▄▅▆▅▄▃▂▁▂▃",
  "▂▁▂▃▄▅▆▇▆▅▄▃▂▁▂"
)
case class BrailSpinner() extends Spinner(FiniteDuration(100, "ms"), "⢎⡰", "⢎⡡", "⢎⡑", "⢎⠱", "⠎⡱", "⢊⡱", "⢌⡱", "⢆⡱")
case class ASCIISpinner() extends Spinner(FiniteDuration(200, "ms"), "|", "/", "—", "\\")
