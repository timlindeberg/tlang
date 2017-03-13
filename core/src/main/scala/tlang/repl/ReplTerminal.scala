package tlang.repl

import java.awt.event._
import java.util.concurrent.TimeUnit

import com.googlecode.lanterna.TextColor.ANSI
import com.googlecode.lanterna.graphics.TextGraphics
import com.googlecode.lanterna.input.KeyStroke
import com.googlecode.lanterna.terminal.swing._
import com.googlecode.lanterna.terminal.{DefaultTerminalFactory, Terminal, TerminalResizeListener}
import com.googlecode.lanterna.{SGR, TerminalPosition, TerminalSize, TextColor}
import tlang.compiler.error._
import tlang.utils.Extensions._


class ReplTerminal(formatting: Formatting) extends Terminal {

  import formatting._
  import formatting.colors._

  private val backingTerminal = createTerminal()

  private val syntaxHighlighter = SyntaxHighlighter(formatting.colors)

  private var boxStartPos       = getCursorPosition
  private var previousBoxHeight = 0

  def onClose(f: => Unit): Unit = {
    backingTerminal.ifInstanceOf[SwingTerminalFrame] { swingTerminal =>
      swingTerminal addWindowListener new WindowAdapter {
        override def windowClosing(windowEvent: WindowEvent): Unit = f
      }
    }
  }

  def putBox(header: String, blocks: List[String]): Int = {
    val box = makeBox(header, blocks)
    put(box)
  }

  def putResultBox(input: String, results: String, success: Boolean): Unit = {
    setCursorPosition(boxStartPos)
    val header = if (success) Green("Result") else Red("Error")
    putBox(Bold(header), highlight(input) :: results :: Nil)
    boxStartPos = getCursorPosition
  }

  def putErrorBox(input: String, errors: List[ErrorMessage]): Unit = {
    setCursorPosition(boxStartPos)
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

    put(sb)
    boxStartPos = getCursorPosition
  }

  def clearLines(num: Int): Unit = {
    val clearLine = (" " * formatting.lineWidth) + "\n"
    put(clearLine * num)
  }

  def putInputBox(commandBuffer: Command): Unit = {
    val input = commandBuffer.text

    setCursorPosition(boxStartPos)

    var linesPut = putBox(Bold(Magenta("Input")), highlight(input) :: Nil)

    val heightDifference = previousBoxHeight - commandBuffer.height
    if (heightDifference > 0) {
      clearLines(heightDifference)
      linesPut += heightDifference
    }

    previousBoxHeight = commandBuffer.height
    boxStartPos = getCursorPosition.withRelativeRow(-linesPut).withColumn(0)
    setCursorPosition(boxStartPos.withRelativeRow(3 + commandBuffer.y).withRelativeColumn(2 + commandBuffer.x))
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
    putBox(header, List(description))
    boxStartPos = getCursorPosition
  }

  def put(chars: IndexedSeq[Char]): Int = {
    var i = 0
    var linesPut = 0
    while (i < chars.size) {
      chars(i) match {
        case '\u001b' if chars(i + 1) == '[' =>
          val endOfAnsi = chars.indexOf('m', i + 1)
          val ansi = chars.subSequence(i + 2, endOfAnsi).toString
          ansi.split(":").map(_.toList).foreach {
            case '0' :: Nil                           => resetColorAndSGR()
            case '1' :: Nil                           => enableSGR(SGR.BOLD)
            case '4' :: Nil                           => enableSGR(SGR.UNDERLINE)
            case '3' :: c :: Nil if c in ('1' to '7') => setForegroundColor(getColor(c))
            case '4' :: c :: Nil if c in ('1' to '7') => setBackgroundColor(getColor(c))
            case _                                    =>
          }
          i = endOfAnsi
        case '\n'                            =>
          linesPut += 1
          putCharacter('\n')
        case c                               => putCharacter(c)
      }
      i += 1
    }
    linesPut
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
  override def pollInput(): KeyStroke = backingTerminal.pollInput()
  override def getCursorPosition: TerminalPosition = backingTerminal.getCursorPosition
  override def flush(): Unit = backingTerminal.flush()
  override def readInput(): KeyStroke = backingTerminal.readInput()

  private def createTerminal() = new DefaultTerminalFactory()
    // .setForceTextTerminal(true)
    .setTerminalEmulatorColorConfiguration(
    TerminalEmulatorColorConfiguration.newInstance(TerminalEmulatorPalette.GNOME_TERMINAL))
    .setTerminalEmulatorFontConfiguration(
      SwingTerminalFontConfiguration.newInstance(new java.awt.Font("Consolas", 0, 16)))
    .setInitialTerminalSize(new TerminalSize(80, 500))
    .createTerminal()
}
