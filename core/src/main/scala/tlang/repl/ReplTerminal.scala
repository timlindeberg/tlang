package tlang.repl

import java.awt.event._
import java.awt.{KeyEventDispatcher, KeyboardFocusManager}
import java.util
import java.util.concurrent.TimeUnit

import com.googlecode.lanterna.TextColor.ANSI
import com.googlecode.lanterna.graphics.TextGraphics
import com.googlecode.lanterna.input.CharacterPattern.Matching
import com.googlecode.lanterna.input.{CharacterPattern, KeyDecodingProfile, KeyStroke, KeyType}
import com.googlecode.lanterna.terminal.ansi.StreamBasedTerminal
import com.googlecode.lanterna.terminal.swing.TerminalEmulatorDeviceConfiguration.CursorStyle
import com.googlecode.lanterna.terminal.swing._
import com.googlecode.lanterna.terminal.{DefaultTerminalFactory, Terminal, TerminalResizeListener}
import com.googlecode.lanterna.{SGR, TerminalPosition, TerminalSize, TextColor}
import tlang.compiler.error._
import tlang.utils.Colors.Color
import tlang.utils.Extensions._

import scala.collection.mutable.ListBuffer


case class ReplTerminal(formatting: Formatting, maxOutputLines: Int) extends Terminal {

  import formatting._
  import formatting.colors._

  private val backingTerminal = createTerminal()

  private val syntaxHighlighter = SyntaxHighlighter(formatting.colors)
  private val wordWrapper       = new AnsiWordWrapper

  private var previousBoxHeight   = 0
  private var boxStartingPosition = getCursorPosition

  private var isShiftDown = false
  private var isCtrlDown  = false
  private var isAltDown   = false

  private val SuccessColor = Bold + Green
  private val ErrorColor   = Bold + Red
  private val MarkedColor  = WhiteBG + Black
  private val InputColor   = Bold + Magenta


  KeyboardFocusManager.getCurrentKeyboardFocusManager addKeyEventDispatcher
    new KeyEventDispatcher() {
      def dispatchKeyEvent(e: KeyEvent): Boolean = {
        isShiftDown = e.isShiftDown
        isCtrlDown = e.isControlDown
        isAltDown = e.isAltDown
        false
      }
    }

  def onClose(f: => Unit): Unit = {
    backingTerminal.ifInstanceOf[SwingTerminalFrame] { swingTerminal =>
      swingTerminal addWindowListener new WindowAdapter {
        override def windowClosing(windowEvent: WindowEvent): Unit = f
      }
    }
  }

  def putBox(chars: IndexedSeq[Char]): Int = {
    setCursorPosition(boxStartingPosition)
    val linesPut = put(chars)
    boxStartingPosition = getCursorPosition
    linesPut
  }

  def putResultBox(input: String, output: String, success: Boolean): Unit = {
    val (color, header) = if (success) (SuccessColor, "Result") else (ErrorColor, "Error")
    val result = truncate(output, color)
    val box = makeBox(color(Bold(header)), highlight(input) :: result :: Nil)
    putBox(box)
  }

  def truncate(output: String, color: Color): String = {
    val wordWrapped = wordWrapper(output, formatting.lineWidth)

    val diff = wordWrapped.size - maxOutputLines
    val lines = wordWrapped.take(maxOutputLines)
    val truncated = if (diff <= 0) lines else lines :+ color(s"... $diff more")
    truncated.mkString("\n")
  }

  def putErrorBox(input: String, errors: List[ErrorMessage]): Unit = {
    val sb = new StringBuilder

    sb ++= makeHeader(ErrorColor("Error"))
    sb ++= divider

    val markings = errors.map { error => Marking(error.pos, Bold + Underline + Red) }

    sb ++= makeLines(syntaxHighlighter(input, markings))


    val errorLines = errors.map { error =>
      val errorFormatter = ErrorFormatter(error, formatting, errorContextSize = 0)
      (errorFormatter.position, errorFormatter.errorPrefix + error.message)
    }
    val diff = errorLines.size - maxOutputLines
    val lines = errorLines.take(maxOutputLines)
    val truncated = if (diff <= 0) lines else lines :+ ("", ErrorColor(s"... $diff more"))


    sb ++= makeBlocksWithColumns(truncated, endOfBlock = true)
    putBox(sb)
  }

  def clearLines(num: Int): Unit = {
    val clearLine = (" " * formatting.lineWidth) + "\n"
    put(clearLine * num)
  }

  def putInputBox(commandBuffer: Command): Unit = {
    val input = commandBuffer.text


    val yIndent = 3
    val xIndent = 2

    val t = if (input.startsWith(":")) InputColor(input) else input
    val markedPos = commandBuffer.getMarkedPosition
    val highlighted = syntaxHighlighter(t, Marking(markedPos, MarkedColor))

    val box = makeBox(InputColor("Input"), highlighted :: Nil)


    setCursorVisible(false)
    var linesPut = putBox(box)

    val heightDifference = previousBoxHeight - commandBuffer.height
    if (heightDifference > 0) {
      clearLines(heightDifference)
      linesPut += heightDifference
    }

    previousBoxHeight = commandBuffer.height

    // Reset position to beginning of box
    boxStartingPosition = getCursorPosition.withRelativeRow(-linesPut).withColumn(0)
    val cursor = commandBuffer.mainCursor
    setCursorPosition(boxStartingPosition.withRelativeRow(yIndent + cursor.y).withRelativeColumn(xIndent + cursor.x))
    setCursorVisible(true)
  }

  def putWelcomeBox(): Unit = {
    val header = Bold("Welcome to the ") + SuccessColor("T-REPL") + Bold("!")
    val description =
      s"""
         |Type in code to have it evaluated or type one of the following commands:
         |   ${InputColor(":help")}
         |   ${InputColor(":quit")}
         |   ${InputColor(":print")}
     """.trim.stripMargin
    val box = makeBox(header, description :: Nil)
    put(box)
    boxStartingPosition = getCursorPosition
  }

  def put(chars: IndexedSeq[Char]): Int = {
    var currentColor = ANSI.DEFAULT
    var currentBGColor = ANSI.DEFAULT
    var SGRs = ListBuffer[SGR]()

    def applyColors() = {
      SGRs.foreach(enableSGR)
      setForegroundColor(currentColor)
      setBackgroundColor(currentBGColor)
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
              resetColorAndSGR()
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
          putCharacter('\n')
        case c                               =>
          x += 1
          putCharacter(c)
      }
      i += 1
    }
    y
  }

  private def highlight(text: String) = if (text.startsWith(":")) InputColor(text) else syntaxHighlighter(text)

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

  override def disableSGR(sgr: SGR): Unit = backingTerminal.disableSGR(sgr)
  override def resetColorAndSGR(): Unit = backingTerminal.resetColorAndSGR()
  override def enterPrivateMode(): Unit = backingTerminal.enterPrivateMode()
  override def newTextGraphics(): TextGraphics = backingTerminal.newTextGraphics()
  override def enquireTerminal(timeout: Int, timeoutUnit: TimeUnit): Array[Byte] = backingTerminal.enquireTerminal(timeout, timeoutUnit)
  override def exitPrivateMode(): Unit = backingTerminal.enterPrivateMode()
  override def enableSGR(sgr: SGR): Unit = backingTerminal.enableSGR(sgr)
  override def setCursorPosition(x: Int, y: Int): Unit = backingTerminal.setCursorPosition(x, y)
  override def setCursorPosition(position: TerminalPosition): Unit = backingTerminal.setCursorPosition(position)
  override def getTerminalSize: TerminalSize = backingTerminal.getTerminalSize
  override def clearScreen(): Unit = backingTerminal.clearScreen()
  override def bell(): Unit = backingTerminal.bell()
  override def setCursorVisible(visible: Boolean): Unit = backingTerminal.setCursorVisible(visible)
  override def putCharacter(c: Char): Unit = backingTerminal.putCharacter(c)
  override def addResizeListener(listener: TerminalResizeListener): Unit = backingTerminal.addResizeListener(listener)
  override def setBackgroundColor(color: TextColor): Unit = backingTerminal.setBackgroundColor(color)
  override def removeResizeListener(listener: TerminalResizeListener): Unit = backingTerminal.removeResizeListener(listener)
  override def setForegroundColor(color: TextColor): Unit = backingTerminal.setForegroundColor(color)
  override def pollInput(): KeyStroke = backingTerminal.pollInput()
  override def readInput(): KeyStroke = {
    val k = backingTerminal.readInput()
    if (k.getKeyType != KeyType.Character) {
      val shift = isShiftDown || k.isShiftDown
      val alt = isAltDown || k.isAltDown
      val ctrl = isCtrlDown || k.isCtrlDown
      new KeyStroke(k.getKeyType, ctrl, alt, shift)
    } else {
      k
    }
  }

  override def getCursorPosition: TerminalPosition = backingTerminal.getCursorPosition
  override def flush(): Unit = backingTerminal.flush()

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

  private def createTerminal() = {
    val term = new DefaultTerminalFactory()
      // .setForceTextTerminal(true)
      .setTerminalEmulatorColorConfiguration(
      TerminalEmulatorColorConfiguration.newInstance(TerminalEmulatorPalette.GNOME_TERMINAL))
      .setTerminalEmulatorFontConfiguration(
        SwingTerminalFontConfiguration.newInstance(new java.awt.Font("Consolas", 0, 16)))
      .setInitialTerminalSize(new TerminalSize(120, 500))
      .setTerminalEmulatorDeviceConfiguration(
        new TerminalEmulatorDeviceConfiguration(50, 1, CursorStyle.VERTICAL_BAR, ANSI.RED, false))
      .createTerminal()

    term.use {
      case streamTerm: StreamBasedTerminal => streamTerm.getInputDecoder.addProfile(CustomKeyProfile)
      case _                               =>
    }
  }
}
