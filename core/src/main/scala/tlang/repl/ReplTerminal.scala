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

  private var boxStartPos        = getCursorPosition
  private var lastInputBoxEndPos = getCursorPosition

  def onClose(f: => Unit): Unit = {
    backingTerminal.ifInstanceOf[SwingTerminalFrame] { swingTerminal =>
      swingTerminal addWindowListener new WindowAdapter {
        override def windowClosing(windowEvent: WindowEvent): Unit = f
      }
    }
  }

  def putBox(header: String, blocks: List[String]): Unit = {
    val box = makeBox(header, blocks)
    put(box)
  }

  def putResultBox(input: String, results: List[String]): Unit = {
    setCursorPosition(boxStartPos)
    putBox(Bold(Green("Result")), highlight(input) :: results.mkString("\n") :: Nil)
    boxStartPos = getCursorPosition
  }

  def putErrorBox(input: String, errors: List[ErrorMessage]): Unit = {
    setCursorPosition(boxStartPos)
    val sb = new StringBuilder

    val markings = errors.map { errors => Marking(errors.pos, Bold + Underline + Red) }
    sb ++= makeHeader(Bold(Red("Error")))
    sb ++= divider
    sb ++= makeLines(syntaxHighlighter(input, markings))

    val lines = errors.map { error =>
      val errorFormatter = ErrorFormatter(error, formatting, errorContextSize = 0)
      (errorFormatter.position, errorFormatter.errorPrefix + error.message)
    }

    sb ++= makeBlocksWithColumns(lines, endOfBlock = true)

    put(sb)
    boxStartPos = getCursorPosition
  }

  def clearLines(num: Int): Unit = {
    val clearLine = (" " * formatting.lineWidth) + "\n"
    put(clearLine * num)
  }

  def putInputBox(commandBuffer: CommandBuffer): Unit = {
    val input = commandBuffer.text
    setCursorPosition(boxStartPos)
    val text = highlight(input)
    putBox(Bold(Magenta("Input")), text :: Nil)
    val currentPos = getCursorPosition

    val diff = lastInputBoxEndPos.getRow - currentPos.getRow
    if (diff > 0)
      clearLines(diff)

    lastInputBoxEndPos = getCursorPosition
    setCursorPosition(boxStartPos.withRelativeRow(3 + commandBuffer.y).withRelativeColumn(2 + commandBuffer.x))
  }

  def putWelcomeBox(): Unit = {
    val header = Bold("Welcome to the ") + Green("T-Repl") + Bold("!")
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

  def put(chars: IndexedSeq[Char]): Unit = {
    var i = 0
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
        case c                               => putCharacter(c)
      }
      i += 1
    }
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
    .createTerminal()
}
