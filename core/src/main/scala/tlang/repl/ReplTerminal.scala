package tlang.repl

import java.awt.KeyboardFocusManager
import java.awt.event._
import java.nio.charset.Charset
import java.util
import scala.collection.JavaConverters._

import better.files.File
import com.googlecode.lanterna.TextColor.ANSI
import com.googlecode.lanterna.input.CharacterPattern.Matching
import com.googlecode.lanterna.input.{CharacterPattern, KeyDecodingProfile, KeyStroke, KeyType}
import com.googlecode.lanterna.terminal.ansi.{UnixLikeTerminal, UnixTerminal}
import com.googlecode.lanterna.terminal.swing.TerminalEmulatorDeviceConfiguration.CursorStyle
import com.googlecode.lanterna.terminal.swing._
import com.googlecode.lanterna.terminal.{DefaultTerminalFactory, Terminal}
import com.googlecode.lanterna.{SGR, TerminalPosition, TerminalSize}
import tlang.formatting.Colors
import tlang.formatting.Colors.{Color, NoColor, extractColorFrom}
import tlang.utils.Extensions._


case class CtrlDown(yes: Boolean)
case class AltDown(yes: Boolean)
case class ShiftDown(yes: Boolean)

trait Key {

  def ctrlDown: CtrlDown
  def altDown: AltDown
  def shiftDown: ShiftDown

  def isCtrlDown: Boolean = ctrlDown.yes
  def isAltDown: Boolean = altDown.yes
  def isShiftDown: Boolean = shiftDown.yes
}

case class CharacterKey(
  char: Char,
  override val ctrlDown: CtrlDown,
  override val altDown: AltDown,
  override val shiftDown: ShiftDown) extends Key

trait Direction
object Direction {
  case object Up extends Direction
  case object Down extends Direction
  case object Left extends Direction
  case object Right extends Direction
}

case class ArrowKey(
  direction: Direction,
  override val ctrlDown: CtrlDown,
  override val altDown: AltDown,
  override val shiftDown: ShiftDown) extends Key

case class OtherKey(
  keyType: KeyType,
  override val ctrlDown: CtrlDown,
  override val altDown: AltDown,
  override val shiftDown: ShiftDown) extends Key


case class ReplTerminal() {

  private val term = createTerminal()

  private var isShiftDown      = false
  private var isCtrlDown       = false
  private var isAltDown        = false
  private var _isCursorVisible = true


  KeyboardFocusManager.getCurrentKeyboardFocusManager.addKeyEventDispatcher { e =>
    isShiftDown = e.isShiftDown
    isCtrlDown = e.isControlDown
    isAltDown = e.isAltDown
    false
  }


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

  def put(str: String): Int = {
    var color: Color = Colors.NoColor
    var (i, y) = (0, 0)

    while (i < str.length) {
      str(i) match {
        case '\u001b' if str(i + 1) == '[' =>
          val (newColor, endOfColor) = extractColorFrom(str, i)
          color += newColor
          i = endOfColor - 1
          applyColor(color)
        case '\n'                          =>
          y += 1
          term.putCharacter('\n')
        case c                             =>
          term.putCharacter(c)
      }
      i += 1
    }

    term.flush()
    y
  }


  def readInput(): Key = {
    val key = term.readInput()

    val ctrl = CtrlDown(isCtrlDown || key.isCtrlDown)
    val alt = AltDown(isAltDown || key.isAltDown)
    val shift = ShiftDown(isShiftDown || key.isShiftDown)

    key.getKeyType match {
      case KeyType.Character  => CharacterKey(key.getCharacter, ctrl, alt, shift)
      case KeyType.Enter      => CharacterKey('\n', ctrl, alt, shift)
      case KeyType.Tab        => CharacterKey('\t', ctrl, alt, shift)
      case KeyType.ArrowUp    => ArrowKey(Direction.Up, ctrl, alt, shift)
      case KeyType.ArrowDown  => ArrowKey(Direction.Down, ctrl, alt, shift)
      case KeyType.ArrowLeft  => ArrowKey(Direction.Left, ctrl, alt, shift)
      case KeyType.ArrowRight => ArrowKey(Direction.Right, ctrl, alt, shift)
      case _                  => OtherKey(key.getKeyType, ctrl, alt, shift)
    }
  }

  def getCursorPosition: TerminalPosition = term.getCursorPosition
  def setCursorPosition(pos: TerminalPosition): Unit = term.setCursorPosition(pos)
  def setCursorVisible(visible: Boolean): Unit = {
    if (visible != _isCursorVisible) {
      _isCursorVisible = visible
      term.setCursorVisible(visible)
    }
  }

  def isCursorVisible: Boolean = _isCursorVisible


  private def applyColor(color: Color): Unit = {
    term.resetColorAndSGR()
    if (color == NoColor)
      return

    color.modifiers.foreach { mod => term.enableSGR(toSGR(mod)) }
    if (color.foreground != -1)
      term.setForegroundColor(toLanternaColor(color.foreground))
    if (color.background != -1)
      term.setForegroundColor(toLanternaColor(color.background))
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
      case (BLACK | BLACK_BG)        => ANSI.BLACK
      case (RED | RED_BG)            => ANSI.RED
      case (GREEN | GREEN_BG)        => ANSI.GREEN
      case (YELLOW | YELLOW_BG)      => ANSI.YELLOW
      case (BLUE | BLUE_BG)          => ANSI.BLUE
      case (MAGENTA | MAGENTA_BG)    => ANSI.MAGENTA
      case (CYAN | CYAN_BG)          => ANSI.CYAN
      case (WHITE | WHITE_BG)        => ANSI.WHITE
      case (DEFAULT_FG | DEFAULT_BG) => ANSI.DEFAULT
    }
  }

  // Translates '[f' to Alt-Right and '[b' to Alt-Left and
  object ForwardBackwardCharacterPattern extends CharacterPattern {
    override def `match`(seq: util.List[Character]): Matching = {
      val size = seq.size

      if(seq.get(0) != KeyDecodingProfile.ESC_CODE)
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

  object CustomKeyProfile extends KeyDecodingProfile {
    override def getPatterns: util.Collection[CharacterPattern] = util.Arrays.asList(ForwardBackwardCharacterPattern)
  }


  private def createTerminal(): Terminal = {
    if (sys.env.get("useTerminalEmulator").contains("true"))
      return createTerminalEmulator()

    val charset = Charset.forName(System.getProperty("file.encoding"))
    new UnixTerminal(System.in, System.out, charset, UnixLikeTerminal.CtrlCBehaviour.TRAP) use {
      _.getInputDecoder.addProfile(CustomKeyProfile)
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
      .createTerminal()

}
