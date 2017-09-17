package tlang.repl.input

import java.awt.Toolkit
import java.awt.datatransfer.{DataFlavor, StringSelection}

case class Clipboard() {

  private val systemClipboard = Toolkit.getDefaultToolkit.getSystemClipboard

  def content: String = systemClipboard.getData(DataFlavor.stringFlavor).asInstanceOf[String]

  def setContent(value: String): Unit = {
    val selection = new StringSelection(value)
    systemClipboard.setContents(selection, selection)
  }

}
