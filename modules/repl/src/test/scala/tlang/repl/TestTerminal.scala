package tlang.repl

import java.util.concurrent.TimeUnit

import com.googlecode.lanterna.TextColor.ANSI
import com.googlecode.lanterna.graphics.TextGraphics
import com.googlecode.lanterna.input.{KeyStroke, KeyType}
import com.googlecode.lanterna.terminal.{Terminal, TerminalResizeListener}
import com.googlecode.lanterna.{SGR, TerminalPosition, TerminalSize, TextColor}
import org.scalatest.Matchers
import tlang.formatting.Colors.Color
import tlang.formatting.{Colors, DefaultFormatting}
import tlang.testutils.AnsiMatchers
import tlang.utils.Extensions._

import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Future, Promise}


/**
  * Class used for doing integration tests on the REPL.
  * The Terminal is both the starting point and the end point of the REPL.
  * It starts with giving Key input (here given through the executeCommand
  * function) and ends by producing an output which can be verified.
  */
class TestTerminal(width: Int, height: Int) extends Terminal {

  private val ExecuteKey  : KeyStroke                   = new KeyStroke(' ', true, false)
  private val terminalSize: TerminalSize                = new TerminalSize(height, width)
  private var cursor      : TerminalPosition            = TerminalPosition.TOP_LEFT_CORNER
  private var color       : Color                       = Colors.NoColor
  private val input       : mutable.Queue[KeyStroke]    = new mutable.Queue[KeyStroke]()
  private val textBuffer  : Array[Array[(Color, Char)]] = Array.tabulate(height, width)((_, _) => (Colors.NoColor, 0))

  private var execution: Option[TestTerminalExecution] = None

  def executeCommand(things: Any*): TestTerminalExecution = {
    val keys = things.flatMap {
      case s: String    => s.map(new KeyStroke(_, false, false))
      case k: KeyStroke => k :: Nil
      case k: KeyType   => new KeyStroke(k, false, false) :: Nil
      case c: Char      => new KeyStroke(c, false, false) :: Nil
      case x            => throw new IllegalArgumentException(s"Invalid argument to executeCommand: $x. Expected a String, KeyStroke or a Char.")
    }
    execute(keys)
  }

  private def execute(keyStrokes: Seq[KeyStroke]): TestTerminalExecution = {
    if (execution.isDefined && !execution.exists(_.finished))
      throw new IllegalStateException("A command is already executing!")

    (keyStrokes :+ ExecuteKey) foreach { input.enqueue(_) }

    new TestTerminalExecution() use { exec => execution = Some(exec) }
  }

  private def lastBox: String = {
    val sb = new StringBuilder
    var previousColor: Color = Colors.NoColor

    // Find start of the last box
    val start = textBuffer.lastIndexWhere { row => row.head._2.toString == DefaultFormatting.TopLeftThick }

    for (i <- start to height) {
      val row = textBuffer(i)
      for ((color, char) <- row) {
        if (char == 0)
          return sb.toString()

        if (color != previousColor) {
          if (previousColor.needsResetBefore(color))
            sb ++= Colors.Reset

          previousColor = color
          sb ++= previousColor
        }

        sb += char
      }

      sb ++= NL
    }
    sb.toString
  }

  // Terminal functions

  override def resetColorAndSGR(): Unit = color = Colors.NoColor
  override def enableSGR(sgr: SGR): Unit = color += convert(sgr)
  override def disableSGR(sgr: SGR): Unit = color -= convert(sgr)
  override def setForegroundColor(c: TextColor): Unit = color += convertFg(c)
  override def setBackgroundColor(c: TextColor): Unit = color += convertBg(c)
  override def flush(): Unit = {
    execution foreach { _.verifyBox(lastBox) }
  }

  override def putCharacter(c: Char): Unit = {
    c match {
      case '\r' => // do nothing
      case '\n' => cursor = cursor.withRelativeRow(1).withColumn(0)
      case _    =>
        textBuffer(cursor.getRow)(cursor.getColumn) = (color, c)
        cursor = cursor.withRelativeColumn(1)
    }
  }

  override def getCursorPosition: TerminalPosition = cursor
  override def setCursorPosition(x: Int, y: Int): Unit = cursor = new TerminalPosition(x, y)
  override def setCursorPosition(position: TerminalPosition): Unit = cursor = position

  override def getTerminalSize: TerminalSize = terminalSize
  override def pollInput(): KeyStroke = if (input.isEmpty) null else input.dequeue()
  override def readInput(): KeyStroke = {

    var input = pollInput()
    while (input == null) {
      Thread.sleep(10)
      input = pollInput()
    }

    input
  }

  // These do nothing

  override def enterPrivateMode(): Unit = {}
  override def exitPrivateMode(): Unit = {}
  override def newTextGraphics(): TextGraphics = null
  override def close(): Unit = {}
  override def enquireTerminal(timeout: Int, timeoutUnit: TimeUnit): Array[Byte] = Array[Byte]()
  override def setCursorVisible(visible: Boolean): Unit = {}
  override def removeResizeListener(listener: TerminalResizeListener): Unit = {}
  override def bell(): Unit = {}
  override def clearScreen(): Unit = {}
  override def addResizeListener(listener: TerminalResizeListener): Unit = {}


  private def convert(sGR: SGR): Color = sGR match {
    case SGR.BOLD      => Colors.Bold
    case SGR.REVERSE   => Colors.Inverse
    case SGR.UNDERLINE => Colors.Underline
    case _             => Colors.NoColor
  }

  private def convertFg(color: TextColor): Color = color match {
    case ANSI.BLACK   => Colors.Black
    case ANSI.RED     => Colors.Red
    case ANSI.GREEN   => Colors.Green
    case ANSI.YELLOW  => Colors.Yellow
    case ANSI.BLUE    => Colors.Blue
    case ANSI.MAGENTA => Colors.Magenta
    case ANSI.CYAN    => Colors.Cyan
    case ANSI.WHITE   => Colors.White
    case ANSI.DEFAULT => Colors.DefaultFG
  }

  private def convertBg(color: TextColor): Color = color match {
    case ANSI.BLACK   => Colors.BlackBG
    case ANSI.RED     => Colors.RedBG
    case ANSI.GREEN   => Colors.GreenBG
    case ANSI.YELLOW  => Colors.YellowBG
    case ANSI.BLUE    => Colors.BlueBG
    case ANSI.MAGENTA => Colors.MagentaBG
    case ANSI.CYAN    => Colors.CyanBG
    case ANSI.WHITE   => Colors.WhiteBG
  }
}


class TestTerminalExecution() extends Matchers with AnsiMatchers {

  private var stopAt: Option[String => Boolean] = None
  private val promise                           = Promise[String]()
  private var lastBox                           = ""
  private val waitTime                          = 10
  private var lastUpdate                        = 0L

  def finished: Boolean = promise.isCompleted

  def stopWhen(stopAt: String => Boolean): this.type = {
    this.stopAt = Some(stopAt)
    this
  }

  def display: Future[String] = {
    if (stopAt.isEmpty)
      throw new IllegalStateException("Cannot call 'display' without calling stopWhen")

    promise.future
  }

  def verifyBox(box: String): Unit = {
    lastBox = box
    if (promise.isCompleted || !stopAt.exists(_.apply(box)))
      return

    val currentTime = System.currentTimeMillis()
    lastUpdate = currentTime
    Future {
      Thread.sleep(waitTime)
      if (lastUpdate == currentTime)
        promise.success(box)
    }
  }

}