package tlang.repl.terminal

import java.awt.event._

import com.googlecode.lanterna.TextColor.ANSI
import com.googlecode.lanterna.input._
import com.googlecode.lanterna.terminal.Terminal
import com.googlecode.lanterna.terminal.swing._
import com.googlecode.lanterna.{SGR, TerminalPosition}
import tlang.formatting.Colors.{Color, NoColor, extractColorFrom}
import tlang.formatting.grid.Grid
import tlang.formatting.{Colors, Formatting}
import tlang.repl._
import tlang.utils.Extensions._

case class ReplTerminal(term: Terminal, formatting: Formatting) {

  private var _isCursorVisible = true

  var boxStartPosition: TerminalPosition = term.getCursorPosition
  var boxHeight       : Int              = 0


  def close(): Unit = {
    term.ifInstanceOf[SwingTerminalFrame] { frame =>
      frame.dispatchEvent(new WindowEvent(frame, WindowEvent.WINDOW_CLOSING))
    }
    term.close()
  }


  def onClose(f: => Unit): Unit = {
    term.ifInstanceOf[SwingTerminalFrame] {
      _ addWindowListener new WindowAdapter {
        override def windowClosing(windowEvent: WindowEvent): Unit = f
      }
    }
  }

  def putBox(grid: Grid, resetStartPosition: Boolean): Unit = {
    term.setCursorPosition(boxStartPosition)

    boxHeight = put(grid.render() + NL)

    val newCursorPosition = term.getCursorPosition

    boxStartPosition = if (resetStartPosition)
      newCursorPosition.withRelativeRow(-boxHeight).withColumn(0)
    else
      newCursorPosition
  }


  def put(str: String): Int = {
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

  def clearScreenFromCursorPosition(): Unit = println("\u001b[0J")

  def clearPreviousDrawing(): Unit = {
    setCursorPosition(boxStartPosition.withRelativeRow(-boxHeight))
    clearScreenFromCursorPosition()
  }

  def readInput(): Key = {
    var key: Option[Key] = None
    while (key.isEmpty)
      key = convertKey(term.readInput())

    key.get
  }

  def getCursorPosition: TerminalPosition = term.getCursorPosition
  def setCursorPosition(pos: TerminalPosition): Unit = term.setCursorPosition(pos)
  def isCursorVisible_=(visible: Boolean): Unit = {
    if (visible == _isCursorVisible)
      return

    _isCursorVisible = visible
    term.setCursorVisible(visible)
  }

  def isCursorVisible: Boolean = _isCursorVisible

  def enableMouseReporting(): Unit = println("\u001b[?1002h")
  def disableMouseReporting(): Unit = println("\u001b[?1002l")


  private def convertKey(key: KeyStroke): Option[Key] = {
    key ifInstanceOf[MouseAction] { mouseAction =>
      return convertMouseEvent(mouseAction)
    }

    val ctrl = Ctrl(key.isCtrlDown)
    val alt = Alt(key.isAltDown)
    val shift = Shift(key.isShiftDown)

    val k = key.getKeyType match {
      case KeyType.Character  => CharacterKey(key.getCharacter, ctrl, alt, shift)
      case KeyType.Enter      => CharacterKey('\n', ctrl, alt, shift)
      case KeyType.Tab        => CharacterKey('\t', ctrl, alt, shift)
      case KeyType.ArrowUp    => ArrowKey(Direction.Up, ctrl, alt, shift)
      case KeyType.ArrowDown  => ArrowKey(Direction.Down, ctrl, alt, shift)
      case KeyType.ArrowLeft  => ArrowKey(Direction.Left, ctrl, alt, shift)
      case KeyType.ArrowRight => ArrowKey(Direction.Right, ctrl, alt, shift)
      case _                  => OtherKey(key.getKeyType, ctrl, alt, shift)
    }
    Some(k)
  }

  private def convertMouseEvent(mouseAction: MouseAction): Option[Key] = {
    val actionType = mouseAction.getActionType

    if (actionType == MouseActionType.SCROLL_DOWN)
      return Some(ArrowKey(Direction.Down, Ctrl(false), Alt(false), Shift(false)))

    if (actionType == MouseActionType.SCROLL_UP)
      return Some(ArrowKey(Direction.Up, Ctrl(false), Alt(false), Shift(false)))

    // We only care about the click and drag event for now
    if (mouseAction.getActionType notIn Seq(MouseActionType.CLICK_DOWN, MouseActionType.DRAG))
      return None

    // Only support left click. CLICK_RELEASE always has button == 0.
    val button = mouseAction.getButton
    if ((actionType in Seq(MouseActionType.CLICK_DOWN, MouseActionType.DRAG)) && button != 1)
      return None


    val startOfBuffer = boxStartPosition
      .withRelativeRow(InputBox.YIndent)
      .withRelativeColumn(InputBox.XIndent)

    val width = 1 + formatting.lineWidth - InputBox.XIndent * 2
    val height = boxHeight - (InputBox.YIndent + 1)

    val mousePos = mouseAction.getPosition
    val x = mousePos.getColumn - startOfBuffer.getColumn
    val y = mousePos.getRow - startOfBuffer.getRow

    mouseAction.getActionType match {
      case MouseActionType.CLICK_DOWN =>
        if ((x notIn (0 until width)) || (y notIn (0 until height)))
          return None

        Some(MouseDown(x, y))
      case MouseActionType.DRAG       =>
        Some(MouseDrag(x.clamp(0, width - 1), y.clamp(0, height - 1)))
      case _                          => ???
    }
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
