package tlang.repl

import java.awt.event.{KeyEvent, _}
import java.awt.{KeyEventDispatcher, KeyboardFocusManager}
import java.util.concurrent.TimeUnit

import com.googlecode.lanterna
import com.googlecode.lanterna.TextColor.ANSI
import com.googlecode.lanterna.graphics.TextGraphics
import com.googlecode.lanterna.terminal.swing.TerminalEmulatorDeviceConfiguration.CursorStyle
import com.googlecode.lanterna.terminal.swing._
import com.googlecode.lanterna.terminal.{DefaultTerminalFactory, Terminal, TerminalResizeListener}
import com.googlecode.lanterna.{SGR, TerminalPosition, TerminalSize, TextColor}
import tlang.compiler.error._
import tlang.utils.Extensions._

import scala.collection.mutable.ListBuffer


class ReplTerminal(formatting: Formatting) extends Terminal {

  import formatting._
  import formatting.colors._

  private val backingTerminal = createTerminal()

  private val syntaxHighlighter = SyntaxHighlighter(formatting.colors)

  private var previousBoxHeight   = 0
  private var boxStartingPosition = getCursorPosition

  private var isShiftDown = false
  private var isCtrlDown  = false
  private var isAltDown   = false

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

  def putBox(chars: IndexedSeq[Char], position: Position = NoPosition): Int = {
    setCursorPosition(boxStartingPosition)
    val linesPut = put(chars, position)
    boxStartingPosition = getCursorPosition
    linesPut
  }

  def putResultBox(input: String, results: String, success: Boolean): Unit = {
    val header = if (success) Green("Result") else Red("Error")
    val box = makeBox(Bold(header), highlight(input) :: results :: Nil)
    putBox(box)
  }

  def putErrorBox(input: String, errors: List[ErrorMessage]): Unit = {
    val sb = new StringBuilder

    sb ++= makeHeader(Bold(Red("Error")))
    sb ++= divider

    val markings = errors.map { error => Marking(error.pos, Bold + Underline + Red) }

    sb ++= makeLines(syntaxHighlighter(input, markings))

    val errorLines = errors.map { error =>
      val errorFormatter = ErrorFormatter(error, formatting, errorContextSize = 0)
      (errorFormatter.position, errorFormatter.errorPrefix + error.message)
    }

    sb ++= makeBlocksWithColumns(errorLines, endOfBlock = true)
    putBox(sb)
  }

  def clearLines(num: Int): Unit = {
    val clearLine = (" " * formatting.lineWidth) + "\n"
    put(clearLine * num)
  }

  def putInputBox(commandBuffer: Command): Unit = {
    val input = commandBuffer.text

    val box = makeBox(Bold(Magenta("Input")), highlight(input) :: Nil)

    val yIndent = 3
    val xIndent = 2

    setCursorVisible(false)
    val pos = commandBuffer.getPosition
    val main = pos.mainCursor.withRelativePos(xIndent, yIndent)
    val secondary = pos.secondaryCursor.withRelativePos(xIndent, yIndent)
    var linesPut = putBox(box, Position(main, secondary))

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
    val header = Bold("Welcome to the ") + Bold(Green("T-REPL")) + Bold("!")
    val description =
      s"""
         |Type in code to have it evaluated or type one of the following commands:
         |   ${Magenta(":help")}
         |   ${Magenta(":quit")}
         |   ${Magenta(":print")}
     """.trim.stripMargin
    val box = makeBox(header, description :: Nil)
    put(box)
    boxStartingPosition = getCursorPosition
  }

  def put(chars: IndexedSeq[Char], position: Position = NoPosition): Int = {
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
          if (position.isWithin(x, y)) {
            resetColorAndSGR()
            setForegroundColor(ANSI.BLACK)
            setBackgroundColor(ANSI.WHITE)
          }
          x += 1
          putCharacter(c)
          applyColors()
      }
      i += 1
    }
    y
  }

  private def highlight(text: String) = if (text.startsWith(":")) Magenta(text) else syntaxHighlighter(text)

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
  override def pollInput(): lanterna.input.KeyStroke = backingTerminal.pollInput()
  override def readInput(): lanterna.input.KeyStroke = backingTerminal.readInput()
  override def getCursorPosition: TerminalPosition = backingTerminal.getCursorPosition
  override def flush(): Unit = backingTerminal.flush()

  def read(): KeyStroke = {
    val key = backingTerminal.readInput()
    KeyStroke(key.getKeyType, key.getCharacter, isCtrlDown, isAltDown, isShiftDown)
  }


  private def createTerminal() = new DefaultTerminalFactory()
    // .setForceTextTerminal(true)
    .setTerminalEmulatorColorConfiguration(
    TerminalEmulatorColorConfiguration.newInstance(TerminalEmulatorPalette.GNOME_TERMINAL))
    .setTerminalEmulatorFontConfiguration(
      SwingTerminalFontConfiguration.newInstance(new java.awt.Font("Consolas", 0, 16)))
    .setInitialTerminalSize(new TerminalSize(80, 500))
    .setTerminalEmulatorDeviceConfiguration(
      new TerminalEmulatorDeviceConfiguration(50, 1, CursorStyle.VERTICAL_BAR, ANSI.RED, false))
    .createTerminal()
}
