package tlang.repl.terminal

import java.awt.KeyboardFocusManager
import java.awt.event._
import java.nio.charset.Charset
import java.util

import com.googlecode.lanterna.TextColor.ANSI
import com.googlecode.lanterna.input.CharacterPattern.Matching
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

  def apply(width: Int): ReplTerminal = ReplTerminal(createTerminal(), width)

  // Translates '[f' to Alt-Right and '[b' to Alt-Left and '^H' to Alt-Backspace
  object ForwardBackwardCharacterPattern extends CharacterPattern {
    override def `match`(seq: util.List[Character]): Matching = {
      val size = seq.size

      if (seq.get(0) != KeyDecodingProfile.ESC_CODE)
        return null

      size match {
        case 1 => Matching.NOT_YET
        case 2 =>
          seq.get(1).charValue() match {
            case 102 => new Matching(new KeyStroke(KeyType.ArrowRight, false, true, false))
            case 98  => new Matching(new KeyStroke(KeyType.ArrowLeft, false, true, false))
            case 8   => new Matching(new KeyStroke(KeyType.Backspace, false, true, false))
            case _   => null
          }
        case _ =>
          null
      }
    }
  }

  private def createTerminal(): Terminal = {
    if (sys.env.get("useTerminalEmulator").contains("true"))
      return createTerminalEmulator()

    val charset = Charset.forName(System.getProperty("file.encoding"))
    new UnixTerminal(System.in, System.out, charset, UnixLikeTerminal.CtrlCBehaviour.TRAP) use { term =>
      term.getInputDecoder.addProfile(() => util.Arrays.asList(ForwardBackwardCharacterPattern))
      term.setMouseCaptureMode(MouseCaptureMode.CLICK_RELEASE_DRAG)
    }
  }

  private def createTerminalEmulator() =
    new DefaultTerminalFactory()
      .setTerminalEmulatorColorConfiguration(
        TerminalEmulatorColorConfiguration.newInstance(TerminalEmulatorPalette.GNOME_TERMINAL))
      .setTerminalEmulatorFontConfiguration(
        new SwingTerminalFontConfiguration(true, AWTTerminalFontConfiguration.BoldMode.EVERYTHING, new java.awt.Font("Meslo LG S", 0, 14))
      )
      .setInitialTerminalSize(new TerminalSize(120, 500))
      .setTerminalEmulatorDeviceConfiguration(
        new TerminalEmulatorDeviceConfiguration(0, 500, CursorStyle.VERTICAL_BAR, ANSI.RED, true))
      .setMouseCaptureMode(MouseCaptureMode.CLICK_RELEASE_DRAG)
      .createTerminal()
}

case class ReplTerminal(term: Terminal, width: Int) {

  private var isShiftDown      = false
  private var isCtrlDown       = false
  private var isAltDown        = false
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
    debug(s"newCursorPosition: $newCursorPosition")

    boxStartPosition = if (resetStartPosition) {
      newCursorPosition.withRelativeRow(-boxHeight).withColumn(0)
    } else {
      newCursorPosition
    }
    debug(s"StartPosition: $boxStartPosition")
    debug(s"Height: $boxHeight")

    boxHeight
  }


  def put(str: String): Int = {
    var color: Color = Colors.NoColor
    var y = 0
    var i = 0
    while (i < str.length) {
      str(i) match {
        case '\u001b' if str(i + 1) == '[' =>
          val (newColor, endOfColor) = extractColorFrom(str, i)
          color += newColor
          i = endOfColor - 1
          applyColor(color)
        case c                             =>
          if(c == '\n')
            y += 1
          term.putCharacter(c)
      }
      i += 1
    }

    term.flush()
    y
  }


  def readInput(): Key = {
    while (true) {
      val key = term.readInput()
      convertKey(key) match {
        case Some(key) =>
          debug(s"Key: $key")
          return key
        case None      =>
      }
    }
    null // Never happens
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

    val ctrl = Ctrl(isCtrlDown || key.isCtrlDown)
    val alt = Alt(isAltDown || key.isAltDown)
    val shift = Shift(isShiftDown || key.isShiftDown)

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

  private def convertMouseEvent(mouseAction: MouseAction): Option[MouseEvent] = {
    val actionType = mouseAction.getActionType

    // We only care about the click and drag event for now
    if (mouseAction.getActionType notIn Seq(MouseActionType.CLICK_DOWN, MouseActionType.DRAG))
      return None

    // Only support left click. CLICK_RELEASE always has button == 0.
    val button = mouseAction.getButton
    if ((actionType in Seq(MouseActionType.CLICK_DOWN, MouseActionType.DRAG)) && button != 1)
      return None

    val pos = mouseAction.getPosition

    val startOfBuffer = boxStartPosition
      .withRelativeRow(InputBox.YIndent)
      .withRelativeColumn(InputBox.XIndent)

    val width = this.width - InputBox.XIndent * 2 + 1
    val height = boxHeight - (InputBox.YIndent + 1)

    val x = pos.getColumn - startOfBuffer.getColumn
    if (x notIn (0 until width))
      return None

    val y = pos.getRow - startOfBuffer.getRow
    if (y notIn (0 until height))
      return None

    val e = mouseAction.getActionType match {
      case MouseActionType.CLICK_DOWN    => MouseDown(x, y)
      case MouseActionType.DRAG          => MouseDrag(x, y)
      case _                             => ???
    }
    Some(e)
  }

  private def applyColor(color: Color): Unit = {
    term.resetColorAndSGR()
    if (color == NoColor)
      return

    color.modifiers.foreach { mod => term.enableSGR(toSGR(mod)) }
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
