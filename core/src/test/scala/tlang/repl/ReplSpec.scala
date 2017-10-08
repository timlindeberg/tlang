package tlang.repl

import java.util.concurrent.TimeUnit

import akka.actor.ActorRef
import com.googlecode.lanterna.TextColor.ANSI
import com.googlecode.lanterna.graphics.TextGraphics
import com.googlecode.lanterna.input.KeyStroke
import com.googlecode.lanterna.terminal.{Terminal, TerminalResizeListener}
import com.googlecode.lanterna.{SGR, TerminalPosition, TerminalSize, TextColor}
import org.scalatest._
import tlang.formatting.Colors.Color
import tlang.formatting.{Colors, DefaultFormatting}
import tlang.options.Options
import tlang.repl.actors.ReplActor.{Start, Stop}
import tlang.testutils.AnsiMatchers
import tlang.utils.Extensions._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.concurrent.{Future, Promise}
import scala.util.Try

class ReplSpec extends AsyncFlatSpec with Matchers with AnsiMatchers with BeforeAndAfterAll {

  def CaptureScreens: Boolean = sys.env("captureScreens").contains("true")

  val Width                      = 80
  val testTerminal: TestTerminal = new TestTerminal(Width, 80)
  var repl        : ActorRef     = _

  override def beforeAll(): Unit = {
    super.beforeAll()
    val formatting = DefaultFormatting.copy(lineWidth = Width)
    repl = Main.createRepl(testTerminal, Options.Empty, formatting)
    repl ! Start
  }

  override def afterAll(): Unit = {
    super.afterAll()
    repl ! Stop
  }

  def key(char: Char) = new KeyStroke(char, false, false)
  def keys(str: String) = str.map(new KeyStroke(_, false, false))

  var testNumber = 1

  private val _currentTestName = new ThreadLocal[String]

  override def withFixture(test: NoArgAsyncTest): FutureOutcome = {
    _currentTestName.set(test.name)
    super.withFixture(test)
  }

  protected def currentTestName: String = _currentTestName.get()

  def matchScreen(found: String, expected: String): Assertion = {
    if (CaptureScreens) {
      println(s"Screen '$currentTestName':$NL$found$NL$NL${ found.escapeAnsi }")
    }

    found should matchWithAnsi(expected)
  }

  it should "execute simple commands" in {
    testTerminal.executeCommand("4 + 4") map { screen =>
      matchScreen(screen,
        s"""|╒══════════════════════════════════════════════════════════════════════════════╕
            |│                            \u001b[1mWelcome to the \u001b[1;32mT-REPL\u001b[0m\u001b[1m!\u001b[0m                            │
            |╞══════════════════════════════════════════════════════════════════════════════╡
            |│                                                                              │
            |├──────────────────────────────────────────────────────────────────────────────┤
            |│ Type in code to have it evaluated or type one of the following commands:     │
            |│                                                                              │
            |│   \u001b[1m•\u001b[0m \u001b[35m:help\u001b[0m                                                                    │
            |│   \u001b[1m•\u001b[0m \u001b[35m:quit\u001b[0m                                                                    │
            |│   \u001b[1m•\u001b[0m \u001b[35m:print\u001b[0m                                                                   │
            |│                                                                              │
            |│ Press \u001b[1;34mCTRL\u001b[0m + \u001b[1;34mSpace\u001b[0m to evaluate the input and \u001b[1;31mCTRL\u001b[0m + \u001b[1;31mC\u001b[0m to exit or type \u001b[35m:quit\u001b[0m. │
            |└──────────────────────────────────────────────────────────────────────────────┘
            |╒══════════════════════════════════════════════════════════════════════════════╕
            |│                                    \u001b[1;32mResult\u001b[0m                                    │
            |╞══════════════════════════════════════════════════════════════════════════════╡
            |│ \u001b[35m4 \u001b[37m+ \u001b[35m4\u001b[0m                                                                          │
            |├──────────────────────────────────────────────────────────────────────────────┤
            |│ \u001b[34mval \u001b[36mres0\u001b[37m: \u001b[36mInt \u001b[37m= \u001b[35m8\u001b[0m                                                            │
            |└──────────────────────────────────────────────────────────────────────────────┘
            |╒══════════════════════════════════════════════════════════════════════════════╕
            |│                                    \u001b[1;35mInput\u001b[0m                                     │
            |╞══════════════════════════════════════════════════════════════════════════════╡
            |│                                                                              │
            |└──────────────────────────────────────────────────────────────────────────────┘
            |""".stripMargin
      )
    }
  }

  it should "use existing variables" in {
    println("execute simple commands2")
    testTerminal.executeCommand("5 * res0") map { screen =>
      matchScreen(screen,
        s"""|╒══════════════════════════════════════════════════════════════════════════════╕
            |│                            \u001b[1mWelcome to the \u001b[1;32mT-REPL\u001b[0m\u001b[1m!\u001b[0m                            │
            |╞══════════════════════════════════════════════════════════════════════════════╡
            |│                                                                              │
            |├──────────────────────────────────────────────────────────────────────────────┤
            |│ Type in code to have it evaluated or type one of the following commands:     │
            |│                                                                              │
            |│   \u001b[1m•\u001b[0m \u001b[35m:help\u001b[0m                                                                    │
            |│   \u001b[1m•\u001b[0m \u001b[35m:quit\u001b[0m                                                                    │
            |│   \u001b[1m•\u001b[0m \u001b[35m:print\u001b[0m                                                                   │
            |│                                                                              │
            |│ Press \u001b[1;34mCTRL\u001b[0m + \u001b[1;34mSpace\u001b[0m to evaluate the input and \u001b[1;31mCTRL\u001b[0m + \u001b[1;31mC\u001b[0m to exit or type \u001b[35m:quit\u001b[0m. │
            |└──────────────────────────────────────────────────────────────────────────────┘
            |╒══════════════════════════════════════════════════════════════════════════════╕
            |│                                    \u001b[1;32mResult\u001b[0m                                    │
            |╞══════════════════════════════════════════════════════════════════════════════╡
            |│ \u001b[35m4 \u001b[37m+ \u001b[35m4\u001b[0m                                                                          │
            |├──────────────────────────────────────────────────────────────────────────────┤
            |│ \u001b[34mval \u001b[36mres0\u001b[37m: \u001b[36mInt \u001b[37m= \u001b[35m8\u001b[0m                                                            │
            |└──────────────────────────────────────────────────────────────────────────────┘
            |╒══════════════════════════════════════════════════════════════════════════════╕
            |│                                    \u001b[1;32mResult\u001b[0m                                    │
            |╞══════════════════════════════════════════════════════════════════════════════╡
            |│ \u001b[35m5\u001b[37m*\u001b[36mres0\u001b[0m                                                                       │
            |├──────────────────────────────────────────────────────────────────────────────┤
            |│ \u001b[34mval \u001b[36mres1\u001b[37m: \u001b[36mInt \u001b[37m= \u001b[35m40\u001b[0m                                                           │
            |└──────────────────────────────────────────────────────────────────────────────┘
            |╒══════════════════════════════════════════════════════════════════════════════╕
            |│                                    \u001b[1;35mInput\u001b[0m                                     │
            |╞══════════════════════════════════════════════════════════════════════════════╡
            |│                                                                              │
            |└──────────────────────────────────────────────────────────────────────────────┘
            |""".stripMargin
      )
    }
  }

  it should "define classes" in {
    println("execute simple commands2")
    testTerminal.executeCommand(
      s"""|class A =
          |\tDef Times(t: Times) =
          |for(var i = 0; i < t: i++)
          |println("A" + i)
          |
          |new A().Times(res2 / 10)
       """.stripMargin
    ) map { screen => matchScreen(screen, "") }
  }


}

class TestTerminal(width: Int, height: Int) extends Terminal {

  private val Execute                                 = new KeyStroke(' ', true, false)
  private val terminalSize                            = new TerminalSize(width, height)
  private var cursor                                  = TerminalPosition.TOP_LEFT_CORNER
  private var color     : Color                       = Colors.NoColor
  private val input     : mutable.Queue[KeyStroke]    = new mutable.Queue[KeyStroke]()
  private val textBuffer: Array[Array[(Color, Char)]] = Array.tabulate(width, height)((_, _) => (Colors.NoColor, 0))

  private val _screens                            = ListBuffer[String]()
  private var isFlushed                           = false
  private var finishExecution: Option[() => Unit] = None

  def executeCommand(command: String): Future[String] = {
    executeCommand(command.map(new KeyStroke(_, false, false)))
  }

  def executeCommand(keyStroke: Seq[KeyStroke]): Future[String] = {
    if (finishExecution.isDefined)
      throw new IllegalStateException("A command is already executing!")

    keyStroke foreach addInput
    addInput(Execute)

    val promise = Promise[String]()

    finishExecution = Some(() => {
      finishExecution = None
      promise.complete(Try(_screens.last))
    })

    promise.future
  }

  def screens: List[String] = _screens.toList

  def content: String = {
    val sb = new StringBuilder
    var color: Color = Colors.NoColor
    for (row <- textBuffer) {
      for ((nextColor, char) <- row) {
        if (char == 0)
          return sb.toString()

        if (nextColor != color) {
          if (color.needsResetBefore(nextColor))
            sb ++= Colors.Reset

          color = nextColor
          sb ++= color
        }

        sb += char
      }

      sb ++= NL
    }
    sb.toString
  }

  def addInput(keyStrokes: KeyStroke*): Unit = keyStrokes.foreach(addInput)
  def addInput(keyStroke: KeyStroke): Unit = input.enqueue(keyStroke)

  override def resetColorAndSGR(): Unit = color = Colors.NoColor
  override def enableSGR(sgr: SGR): Unit = color += convert(sgr)
  override def disableSGR(sgr: SGR): Unit = color -= convert(sgr)
  override def setForegroundColor(c: TextColor): Unit = color += convertFg(c)
  override def setBackgroundColor(c: TextColor): Unit = color += convertBg(c)
  override def flush(): Unit = {
    _screens += content
  }

  override def putCharacter(c: Char): Unit = {
    isFlushed = false
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
  override def pollInput(): KeyStroke = {
    if (input.isEmpty) return null
    Thread.sleep(100)
    input.dequeue() use {
      _ ifMatches {
        case k: KeyStroke if k eq Execute => finishExecution.foreach(_.apply)
      }
    }
  }
  override def readInput(): KeyStroke = {
    var input = pollInput()
    while (input == null) {
      Thread.sleep(100)
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
    case SGR.UNDERLINE => Colors.Underlined
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
    case ANSI.DEFAULT => Colors.DefaultBG
  }
}
