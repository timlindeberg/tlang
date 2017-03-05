package tlang.repl

import java.awt.Toolkit
import java.awt.datatransfer.{Clipboard, DataFlavor}
import java.io.{File, FileWriter}

import com.googlecode.lanterna.input.{KeyStroke, KeyType}
import tlang.compiler.Context
import tlang.compiler.error.{CompilationException, ErrorLevel, ErrorMessages}
import tlang.utils.Extensions._
import tlang.utils.{Enumerable, Enumeration}

import scala.collection.mutable.ListBuffer
import scalaz.Cord

/**
  * Created by Tim Lindeberg on 2/25/2017.
  */
case class ReplLoop(ctx: Context) {
  
  private val MaxRedoSize       = 500
  private val TabSize           = 4
  private val HistorySeperator  = "★"
  private val SettingsDirectory = System.getProperty("user.home") + File.separator + ".tlang"
  private val HistoryFileName   = "repl_history"

  private val historyFile    = new File(SettingsDirectory, HistoryFileName)
  private val replProgram    = ReplProgram(ctx)
  private val terminal       = new ReplTerminal(ctx.formatting)
  private var running        = false
  private val commandBuffers = CircularBuffer[CommandBuffer](CommandBuffer(MaxRedoSize, TabSize))

  private def currentCommand = commandBuffers()

  def start(): Unit = {

    sys.addShutdownHook {
      saveHistory()
    }

    running = true
    loadHistory()

    terminal.onClose {running = false}
    terminal.putWelcomeBox()

    try {
      loop()
    } finally {
      saveHistory()
    }
  }

  private def loop() =
    while (running) {
      terminal.putInputBox(currentCommand)
      terminal.flush()

      val keyStroke = terminal.readInput()

      Commands.find(_.matches(keyStroke)) match {
        case Some(command) => command.execute(keyStroke)
        case None          =>
          val c = keyStroke.getCharacter
          if (c != null) currentCommand += c
      }
    }


  private def loadHistory(): Unit = {
    if (historyFile.getParentFile.mkdirs() || historyFile.createNewFile())
      return

    val lines = new ListBuffer[String]()
    using(io.Source.fromFile(historyFile)) {
      _.getLines().foreach { line =>
        if (line == HistorySeperator && lines.nonEmpty) {
          val cord = Cord.empty :+ lines.mkString("\n")
          commandBuffers += CommandBuffer(MaxRedoSize, TabSize, cord)
          lines.clear()
        } else {
          lines += line
        }
      }
    }
  }

  private def saveHistory(): Unit =
    using(new FileWriter(historyFile, false)) {
      _.write(
        commandBuffers
          .drop(1)
          .map(_.cord.toString())
          .mkString("\n" + HistorySeperator + "\n")
      )
    }

  private def saveCommand(command: String): Unit = {
    // Reset base command buffer
    commandBuffers.setIndex(0)
    currentCommand.clear()

    // Remove previous if it already existed
    commandBuffers.find(_.cord.toString() == command) ifDefined { alreadyExisting =>
      commandBuffers -= alreadyExisting
    }

    commandBuffers += CommandBuffer(MaxRedoSize, TabSize, Cord.empty :+ command)
    using(new FileWriter(historyFile, true)) {_.write(command + "\n" + HistorySeperator + "\n")}
  }

  sealed abstract class Command() extends Product with Serializable {
    def matches(keyStroke: KeyStroke): Boolean
    def execute(keyStroke: KeyStroke): Unit
  }

  object Commands extends Enumerable[Command] {

    private def largeMovement(keyStroke: KeyStroke): Boolean = keyStroke.isAltDown

    case object Evaluate extends Command {

      override def matches(keyStroke: KeyStroke): Boolean =
        keyStroke.getKeyType == KeyType.Enter && keyStroke.isCtrlDown

      override def execute(keyStroke: KeyStroke): Unit = {
        val command = currentCommand.text
        if (command.nonEmpty) {
          saveCommand(command)
          evaluate(command) match {
            case Right(messages)     => terminal.putResultBox(command, messages)
            case Left(errorMessages) => terminal.putErrorBox(command, errorMessages(ErrorLevel.Error))
          }
        }
      }

      def evaluate(command: String): Either[ErrorMessages, List[String]] = {
        if (!command.startsWith(":")) {
          try {
            return Right(replProgram.execute(command))
          } catch {
            case e: CompilationException => return Left(e.messages)
          }

        }

        val message = command.drop(1) match {
          case "help"  => "LOL NO HELP FOR U"
          case "quit"  =>
            running = false
            "Exiting..."
          case "print" =>
            replProgram.prettyPrinted.trimWhiteSpaces
          case _       => s"Command not supported: $command"
        }
        Right(List(message))
      }

    }

    case object Undo extends Command {
      override def matches(keyStroke: KeyStroke): Boolean =
        keyStroke.isCtrlDown && !keyStroke.isAltDown && keyStroke.getCharacter == 'z'

      override def execute(keyStroke: KeyStroke): Unit = currentCommand.undo()
    }

    case object Redo extends Command {
      override def matches(keyStroke: KeyStroke): Boolean =
        keyStroke.isCtrlDown && keyStroke.isAltDown && keyStroke.getCharacter == 'z'

      override def execute(keyStroke: KeyStroke): Unit = currentCommand.redo()
    }


    case object Remove extends Command {
      override def matches(keyStroke: KeyStroke): Boolean = keyStroke.getKeyType == KeyType.Backspace

      override def execute(keyStroke: KeyStroke): Unit =
        if (largeMovement(keyStroke)) currentCommand.removeToLeftWord() else currentCommand.remove()
    }

    case object GoLeft extends Command {
      override def matches(keyStroke: KeyStroke): Boolean = keyStroke.getKeyType == KeyType.ArrowLeft

      override def execute(keyStroke: KeyStroke): Unit = {
        if (largeMovement(keyStroke)) currentCommand.goToLeftWord() else currentCommand.moveLeft(1)
      }
    }

    case object GoRight extends Command {
      override def matches(keyStroke: KeyStroke): Boolean = keyStroke.getKeyType == KeyType.ArrowRight

      override def execute(keyStroke: KeyStroke): Unit = {
        if (largeMovement(keyStroke)) currentCommand.goToRightWord() else currentCommand.moveRight(1)
      }
    }

    case object GoUp extends Command {
      override def matches(keyStroke: KeyStroke): Boolean = keyStroke.getKeyType == KeyType.ArrowUp

      override def execute(keyStroke: KeyStroke): Unit = {
        if (!currentCommand.up())
          commandBuffers -= 1
      }
    }

    case object GoDown extends Command {
      override def matches(keyStroke: KeyStroke): Boolean = keyStroke.getKeyType == KeyType.ArrowDown

      override def execute(keyStroke: KeyStroke): Unit = {
        if (!currentCommand.down())
          commandBuffers += 1
      }
    }

    case object Paste extends Command {

      override def matches(keyStroke: KeyStroke): Boolean = keyStroke.isCtrlDown && keyStroke.getCharacter == 'v'

      override def execute(keyStroke: KeyStroke): Unit = currentCommand ++= clipboardContents

      private val systemClipboard: Clipboard = Toolkit.getDefaultToolkit.getSystemClipboard
      private def clipboardContents: String = systemClipboard.getData(DataFlavor.stringFlavor).asInstanceOf[String]
    }

    protected lazy val All: Set[Command] = Enumeration.instancesOf[Command]
  }


}
