package tlang.repl

import com.googlecode.lanterna.SGR
import com.googlecode.lanterna.TextColor.ANSI
import com.googlecode.lanterna.input.{KeyStroke, KeyType}
import com.googlecode.lanterna.terminal._
import tlang.compiler.ast.PrettyPrinter
import tlang.compiler.error.CompilationException
import tlang.utils.Colors
import tlang.utils.Extensions._

/**
  * Created by Tim Lindeberg on 2/13/2017.
  */
object Main {

  var running = true

  val replProgram        = ReplProgram()
  val printer            = PrettyPrinter(Colors(true))
  val terminal: Terminal = new DefaultTerminalFactory().createTerminal()

  var history = new StringBuilder

  def main(args: Array[String]): Unit = {
    val commandBuffer = new StringBuilder
    while (running) {
      val keyStroke: KeyStroke = terminal.readInput()
      terminal.clearScreen()

      keyStroke match {
        case CommandEntry() =>
          val command = commandBuffer.toString()
          commandBuffer.clear()
          val res = executeCommand(command)
          res.foreach(history.append(_))
        case Backspace()    =>
          if (commandBuffer.nonEmpty)
            commandBuffer.deleteCharAt(commandBuffer.length - 1)
        case _              =>
          val c = keyStroke.getCharacter
          if (c != null)
            commandBuffer += c
      }

      put(history)
      put(commandBuffer)
      terminal.flush()
    }
  }

  def put(chars: IndexedSeq[Char]): Unit = {
    var i = 0
    while (i < chars.size) {
      chars(i) match {
        case '\u001b' if chars(i + 1) == '[' =>
          val endOfAnsi = chars.indexOf('m', i + 1)
          val ansi = chars.subSequence(i + 2, endOfAnsi).toString
          ansi.split(":").map(_.toList).foreach {
            case '0' :: Nil                           => terminal.resetColorAndSGR()
            case '1' :: Nil                           => terminal.enableSGR(SGR.BOLD)
            case '4' :: Nil                           => terminal.enableSGR(SGR.UNDERLINE)
            case '3' :: c :: Nil if c in ('1' to '7') => terminal.setForegroundColor(getColor(c))
            case '4' :: c :: Nil if c in ('1' to '7') => terminal.setBackgroundColor(getColor(c))
            case _                                    =>
          }
          i = endOfAnsi
        case c                               => terminal.putCharacter(c)
      }
      i += 1
    }
  }

  def executeCommand(command: String): List[String] = {
    if (!command.startsWith(":")) {
      try {
        return replProgram.execute(command)
      } catch {
        case e: CompilationException => return List(e.getMessage)
      }

    }
    command.drop(1) match {
      case "help"  =>
        List("LOL NO HELP FOR U")
      case "quit"  =>
        running = false
        List("Exiting...")
      case "print" =>
        List(printer(replProgram.generateCompilationUnit))
      case _       => List(s"Command not supported: $command")
    }
  }

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

  object CommandEntry {
    def unapply(keyStroke: KeyStroke): Boolean = keyStroke.isCtrlDown && keyStroke.getKeyType == KeyType.Enter
  }
  object Backspace {
    def unapply(keyStroke: KeyStroke): Boolean = keyStroke.getKeyType == KeyType.Backspace
  }

}
