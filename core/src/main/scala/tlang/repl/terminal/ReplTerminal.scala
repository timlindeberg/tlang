package tlang.repl.terminal

import java.awt.event._
import java.nio.charset.Charset

import com.googlecode.lanterna.TextColor.ANSI
import com.googlecode.lanterna.input._
import com.googlecode.lanterna.terminal.ansi.{UnixLikeTerminal, UnixTerminal}
import com.googlecode.lanterna.terminal.swing.TerminalEmulatorDeviceConfiguration.CursorStyle
import com.googlecode.lanterna.terminal.swing._
import com.googlecode.lanterna.terminal.{DefaultTerminalFactory, MouseCaptureMode, Terminal}
import com.googlecode.lanterna.{SGR, TerminalPosition, TerminalSize}
import tlang.formatting.Colors
import tlang.formatting.Colors.{Color, NoColor, extractColorFrom}
import tlang.formatting.grid.Grid
import tlang.repl._
import tlang.utils.Extensions._


object ReplTerminal {

  def apply(width: Int): ReplTerminal = ReplTerminal(underlyingTerminal(), width)


  private def underlyingTerminal(): Terminal = {
    if (sys.env.get("useTerminalEmulator").contains("true"))
      return createTerminalEmulator()

    val charset = Charset.forName(System.getProperty("file.encoding"))
    new UnixTerminal(System.in, System.out, charset, UnixLikeTerminal.CtrlCBehaviour.TRAP) use { term =>
      term.getInputDecoder.addProfile(CustomCharacterPatterns)
    }
  }

  private def createTerminalEmulator() =
    new DefaultTerminalFactory()
      .setTerminalEmulatorColorConfiguration(emulatorColors)
      .setTerminalEmulatorFontConfiguration(emulatorFont)
      .setInitialTerminalSize(new TerminalSize(80, 50))
      .setTerminalEmulatorDeviceConfiguration(deviceConfiguration)
      .setMouseCaptureMode(MouseCaptureMode.CLICK_RELEASE_DRAG)
      .createTerminal()

  private lazy val emulatorFont =
    new SwingTerminalFontConfiguration(
      true,
      AWTTerminalFontConfiguration.BoldMode.EVERYTHING,
      new java.awt.Font("Meslo LG S", 0, 14)
    )

  private lazy val deviceConfiguration =
    new TerminalEmulatorDeviceConfiguration(0, 500, CursorStyle.VERTICAL_BAR, ANSI.RED, true)

  private lazy val emulatorColors = TerminalEmulatorColorConfiguration.newInstance(new TerminalEmulatorPalette(
    new java.awt.Color(177, 204, 217), // defaultColor
    new java.awt.Color(177, 204, 217), // defaultBrightColor
    new java.awt.Color(50, 65, 72), //    defaultBackgroundColor
    new java.awt.Color(65, 87, 98), //    normalBlack
    new java.awt.Color(100, 133, 146), // brightBlack
    new java.awt.Color(247, 140, 108), // normalRed
    new java.awt.Color(255, 83, 112), //  brightRed
    new java.awt.Color(195, 232, 141), // normalGreen
    new java.awt.Color(204, 247, 175), // brightGreen
    new java.awt.Color(255, 203, 107), // normalYellow
    new java.awt.Color(255, 203, 67), //  brightYellow
    new java.awt.Color(130, 170, 255), // normalBlue
    new java.awt.Color(137, 221, 255), // brightBlue
    new java.awt.Color(199, 146, 234), // normalMagenta
    new java.awt.Color(207, 160, 237), // brightMagenta
    new java.awt.Color(147, 233, 217), // normalCyan
    new java.awt.Color(0, 185, 204), //   brightCyan
    new java.awt.Color(247, 247, 247), // normalWhite
    new java.awt.Color(255, 255, 255) //  brightWhite
  ))
}

case class ReplTerminal(term: Terminal, width: Int) {

  private var _isCursorVisible = true

  var boxStartPosition: TerminalPosition = term.getCursorPosition
  var boxHeight       : Int              = 0


  def close(): Unit =
    term.ifInstanceOf[SwingTerminalFrame] { frame =>
      frame.dispatchEvent(new WindowEvent(frame, WindowEvent.WINDOW_CLOSING))
    }

  def onClose(f: => Unit): Unit = {
    term.ifInstanceOf[SwingTerminalFrame] {
      _ addWindowListener new WindowAdapter {
        override def windowClosing(windowEvent: WindowEvent): Unit = f
      }
    }
  }

  def putBox(grid: Grid, resetStartPosition: Boolean): Int = {
    term.setCursorPosition(boxStartPosition)

    boxHeight = put(grid.render() + NL)

    val newCursorPosition = term.getCursorPosition

    boxStartPosition = if (resetStartPosition) {
      newCursorPosition.withRelativeRow(-boxHeight).withColumn(0)
    } else {
      newCursorPosition
    }

    boxHeight
  }


  def put(str: String): Int = {
    var color: Color = Colors.NoColor
    var y = 0
    var i = 0
    while (i < str.length) {
      str(i) match {
        case '\u001b' if str(i + 1) == '[' =>
          val (newColor, endOfColor) = extractColorFrom(str, i, extractMultiple = false)
          color += newColor
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

    if(actionType == MouseActionType.SCROLL_DOWN)
      return Some(ArrowKey(Direction.Down, Ctrl(false), Alt(false), Shift(false)))

    if(actionType == MouseActionType.SCROLL_UP)
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

    val width = 1 + this.width - InputBox.XIndent * 2
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
