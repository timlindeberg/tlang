package tlang.repl

import java.awt.event._
import java.awt.{KeyEventDispatcher, KeyboardFocusManager}
import java.nio.charset.Charset
import java.util

import com.googlecode.lanterna.TextColor.ANSI
import com.googlecode.lanterna.input.CharacterPattern.Matching
import com.googlecode.lanterna.input.{CharacterPattern, KeyDecodingProfile, KeyStroke, KeyType}
import com.googlecode.lanterna.terminal.ansi.{UnixLikeTerminal, UnixTerminal}
import com.googlecode.lanterna.terminal.swing.TerminalEmulatorDeviceConfiguration.CursorStyle
import com.googlecode.lanterna.terminal.swing._
import com.googlecode.lanterna.terminal.{DefaultTerminalFactory, Terminal}
import com.googlecode.lanterna.{SGR, TerminalPosition, TerminalSize}
import tlang.utils.Extensions._

import scala.collection.mutable.ListBuffer

class ReplTerminal {

  private val term = createTerminal()

  private var isShiftDown      = false
  private var isCtrlDown       = false
  private var isAltDown        = false
  private var _isCursorVisible = true


  KeyboardFocusManager.getCurrentKeyboardFocusManager addKeyEventDispatcher
  new KeyEventDispatcher() {
    def dispatchKeyEvent(e: KeyEvent): Boolean = {
      isShiftDown = e.isShiftDown
      isCtrlDown = e.isControlDown
      isAltDown = e.isAltDown
      false
    }
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

  def put(chars: IndexedSeq[Char]): Int = {
    var currentColor = ANSI.DEFAULT
    var currentBGColor = ANSI.DEFAULT
    var SGRs = ListBuffer[SGR]()

    def applyColors() = {
      SGRs.foreach(term.enableSGR)
      term.setForegroundColor(currentColor)
      term.setBackgroundColor(currentBGColor)
    }

    var i = 0
    var x = 0
    var y = 0
    while (i < chars.size) {
      chars(i) match {
        case '\u001b' if chars(i + 1) == '[' =>
          val endOfAnsi = chars.indexOf('m', i + 1)
          val ansi = chars.subSequence(i + 2, endOfAnsi).toString
          ansi.split(":").map(_.toList).foreach {
            case '0' :: Nil                           =>
              currentBGColor = ANSI.DEFAULT
              currentColor = ANSI.DEFAULT
              SGRs.clear()
              term.resetColorAndSGR()
            case '1' :: Nil                           => SGRs += SGR.BOLD
            case '4' :: Nil                           => SGRs += SGR.UNDERLINE
            case '3' :: c :: Nil if c in ('1' to '7') => currentColor = getColor(c)
            case '4' :: c :: Nil if c in ('1' to '7') => currentBGColor = getColor(c)
            case _                                    =>
          }
          applyColors()
          i = endOfAnsi
        case '\n'                            =>
          y += 1
          x = 0
          term.putCharacter('\n')
        case c                               =>
          x += 1
          term.putCharacter(c)
      }
      i += 1
    }
    term.flush()
    y
  }

  def readInput(): KeyStroke = {
    val k = term.readInput()
    if (k.getKeyType != KeyType.Character) {
      val shift = isShiftDown || k.isShiftDown
      val alt = isAltDown || k.isAltDown
      val ctrl = isCtrlDown || k.isCtrlDown
      new KeyStroke(k.getKeyType, ctrl, alt, shift)
    } else {
      k
    }
  }

  def getCursorPosition: TerminalPosition = term.getCursorPosition
  def setCursorPosition(pos: TerminalPosition): Unit = term.setCursorPosition(pos)
  def setCursorVisible(visible: Boolean): Unit = {
    _isCursorVisible = visible
    term.setCursorVisible(visible)
  }

  def isCursorVisible: Boolean = _isCursorVisible

  private def getColor(char: Char) = char match {
    case '0' => ANSI.BLACK
    case '1' => ANSI.RED
    case '2' => ANSI.GREEN
    case '3' => ANSI.YELLOW
    case '4' => ANSI.BLUE
    case '5' => ANSI.MAGENTA
    case '6' => ANSI.CYAN
    case '7' => ANSI.WHITE
    case _   => ???
  }


  // Translates '[f' to Alt-Right and '[b' to Alt-Left
  object ForwardBackwardCharacterPattern extends CharacterPattern {
    override def `match`(seq: util.List[Character]): Matching = {
      val size = seq.size
      if (size > 2 || seq.get(0) != KeyDecodingProfile.ESC_CODE)
        return null

      if (size == 1)
        return Matching.NOT_YET

      seq.get(1).charValue() match {
        case 'f' => new Matching(new KeyStroke(KeyType.ArrowRight, false, true, false))
        case 'b' => new Matching(new KeyStroke(KeyType.ArrowLeft, false, true, false))
        case _   => null
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
