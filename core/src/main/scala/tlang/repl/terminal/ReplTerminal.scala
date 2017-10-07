package tlang.repl.terminal

import java.awt.event._

import com.googlecode.lanterna.TextColor.ANSI
import com.googlecode.lanterna.input._
import com.googlecode.lanterna.terminal.Terminal
import com.googlecode.lanterna.terminal.swing._
import com.googlecode.lanterna.{SGR, TerminalPosition}
import tlang.formatting.Colors.{Color, NoColor, extractColorFrom}
import tlang.formatting.{Colors, Formatting}
import tlang.repl.OutputBox.{TabWidth, XIndent, YIndent}
import tlang.repl._
import tlang.repl.input.InputBuffer
import tlang.utils.Extensions._


object ReplTerminal {

  val MouseReportingDragClick = "\u001b[?1002"
  val MouseReportingDecimals  = "\u001b[?1005"

}

case class ReplTerminal(term: Terminal, keyConverter: KeyConverter, formatting: Formatting) {

  import ReplTerminal._

  private var _isCursorVisible      = true
  private var _enableMouseReporting = false
  private var _width: Int           = formatting.lineWidth

  var boxStartPosition : TerminalPosition = cursorPosition
  var boxHeight        : Int              = 0
  var previousBoxHeight: Int              = 0


  def close(): Unit = {
    term.ifInstanceOf[SwingTerminalFrame] { frame =>
      frame.dispatchEvent(new WindowEvent(frame, WindowEvent.WINDOW_CLOSING))
    }
    enableMouseReporting = false
    term.close()
  }


  def onClose(f: => Unit): Unit = {
    term.ifInstanceOf[SwingTerminalFrame] {
      _ addWindowListener new WindowAdapter {
        override def windowClosing(windowEvent: WindowEvent): Unit = f
      }
    }
  }

  def newBox(outputBox: OutputBox): Unit = {
    putBox(outputBox)
    boxStartPosition = cursorPosition
    cursorPosition = getCursorsPositionWithinBox()
  }

  def updateBox(outputBox: OutputBox): Unit = {
    val cursorPos = cursorPosition
    putBox(outputBox)
    cursorPosition = cursorPos
  }

  def updateCursor(inputBuffer: InputBuffer): Unit = {
    val cursor = inputBuffer.mainCursor
    val currentLine = inputBuffer.currentLine
    val tabsBeforeCursor = currentLine.take(cursor.x).count(_ == '\t')
    val lineLength = currentLine.length

    val xOffset = cursor.x + (TabWidth - 1) * tabsBeforeCursor
    val yOffset = cursor.y
    cursorPosition = getCursorsPositionWithinBox(xOffset, yOffset)

    // To make room for truncation
    val boxSpace = formatting.lineWidth - 2 * XIndent
    val end = if (lineLength > boxSpace) boxSpace - 3 else boxSpace
    val isCursorInsideBox = cursor.x <= end
    isCursorVisible = isCursorInsideBox
  }

  def width: Int = _width
  def width_=(newWidth: Int): Unit = {
    if (_width > newWidth) {
      cursorPosition = boxStartPosition.withRelativeRow(-boxHeight)
      clearScreenFromCursorPosition()
    }
    _width = newWidth
  }

  def clearScreenFromCursorPosition(): Unit = {
    print("\u001b[0J")
    System.out.flush()
  }

  def readInput(): Key = {
    var key: Option[Key] = None
    while (key.isEmpty) {
      key = term.readInput() match {
        case m: MouseAction => keyConverter.convertMouseEvent(m, boxStartPosition, formatting.lineWidth, boxHeight)
        case key            => keyConverter.convertKey(key)
      }
    }
    key.get
  }

  def cursorPosition: TerminalPosition = term.getCursorPosition
  def cursorPosition_=(pos: TerminalPosition): Unit = term.setCursorPosition(pos)

  def isCursorVisible: Boolean = _isCursorVisible
  def isCursorVisible_=(visible: Boolean): Unit = {
    if (visible == _isCursorVisible)
      return

    _isCursorVisible = visible
    term.setCursorVisible(visible)
  }


  def enableMouseReporting: Boolean = _enableMouseReporting
  def enableMouseReporting_=(enable: Boolean): Unit = {
    if (_enableMouseReporting == enable)
      return

    val c = if (enable) "h" else "l"
    print(MouseReportingDragClick + c)
    print(MouseReportingDecimals + c)

    _enableMouseReporting = enable
  }

  private def putBox(outputBox: OutputBox): Unit = {
    cursorPosition = boxStartPosition

    boxHeight = put(outputBox.render())

    if (previousBoxHeight - boxHeight > 0)
      clearScreenFromCursorPosition()

    previousBoxHeight = boxHeight
  }

  private def put(str: String): Int = {
    var y = 0
    var i = 0

    while (i < str.length) {
      str(i) match {
        case '\u001b' if str(i + 1) == '[' =>
          val (color, endOfColor) = extractColorFrom(str, i, extractMultiple = false)
          i = endOfColor - 1
          applyColor(color)
        case c                             =>
          if (c == '\n')
            y += 1
          term.putCharacter(c)
      }
      i += 1
    }

    term.flush()
    y
  }

  private def getCursorsPositionWithinBox(xOffset: Int = 0, yOffset: Int = 0) = {
    boxStartPosition
      .withRelativeColumn(XIndent + xOffset)
      .withRelativeRow(YIndent + yOffset)
  }


  private def applyColor(color: Color): Unit = {
    if (color == NoColor) {
      term.resetColorAndSGR()
      return
    }

    color.modifiers.map(toSGR).foreach(term.enableSGR)
    if (color.foreground != -1)
      term.setForegroundColor(toLanternaColor(color.foreground))
    if (color.background != -1)
      term.setBackgroundColor(toLanternaColor(color.background))
  }


  private def toSGR(color: Int): SGR = color match {
    case Colors.BOLD       => SGR.BOLD
    case Colors.UNDERLINED => SGR.UNDERLINE
    case Colors.BLINK      => SGR.BLINK
    case Colors.INVERSE    => SGR.REVERSE
    case _                 => ???
  }

  private def toLanternaColor(color: Int): ANSI = {
    import Colors._
    color match {
      case BLACK | BLACK_BG        => ANSI.BLACK
      case RED | RED_BG            => ANSI.RED
      case GREEN | GREEN_BG        => ANSI.GREEN
      case YELLOW | YELLOW_BG      => ANSI.YELLOW
      case BLUE | BLUE_BG          => ANSI.BLUE
      case MAGENTA | MAGENTA_BG    => ANSI.MAGENTA
      case CYAN | CYAN_BG          => ANSI.CYAN
      case WHITE | WHITE_BG        => ANSI.WHITE
      case DEFAULT_FG | DEFAULT_BG => ANSI.DEFAULT
    }
  }


}
