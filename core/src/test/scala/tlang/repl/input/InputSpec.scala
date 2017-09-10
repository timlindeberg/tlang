package tlang.repl.input

import java.awt.datatransfer.Clipboard

import better.files.File
import tlang.testutils.UnitSpec

// When testing the InputHistory class we should maybe mock more of it's dependencies
// such as CircularBuffer and InputBuffer but it makes writing the tests really difficult
class InputSpec extends UnitSpec {

  import Input._


  val DefaultMaxHistory = 25
  val DefaultTabSize    = 2


  behavior of "Input history"


  it should "create a new history file if it does not exist" in {
    val historyFile = mock[File]
    historyFile.exists returns false

    createInput(historyFile)

    there was one(historyFile).createIfNotExists()
  }

  it should "save input history to an empty file" in {
    val (fileContents, file) = memoryFile()


    val input = createInput(file)

    input.saveToFile()
    fileContents.toString shouldBe ""


    input ++= "ABC"
    input.saveCurrent()
    input.saveToFile()
    fileContents.toString shouldBe s"ABC${ Seperator }"


    input ++= "DEF"
    input.saveCurrent()
    input.saveToFile()
    fileContents.toString shouldBe s"ABC${ Seperator }DEF${ Seperator }"

    input ++= "GHI"
    input.saveCurrent()
    input.saveToFile()
    fileContents.toString shouldBe s"ABC${ Seperator }DEF${ Seperator }GHI${ Seperator }"
  }

  it should "not write the same content to file twice" in {
    val (fileContents, file) = memoryFile()


    val input = createInput(file)

    fileContents.toString shouldBe ""


    input ++= "ABC"
    input.saveCurrent()

    input.saveToFile()
    input.saveToFile()

    there was one(file).write(*)(*, *)
  }


  it should "not write to file unless changes have been saved" in {
    val (fileContents, file) = memoryFile()


    val input = createInput(file)

    input ++= "ABC"
    input.saveToFile()

    there was no(file).write(*)(*, *)
  }

  it should "load input history from file" in {
    val (_, file) = memoryFile(
      s"""|ABCDEFGH
          |$HistorySeperator
          |A
          |B
          |C
          |D
          |$HistorySeperator
          |ABCDEF
          |GHIJKL
          |$HistorySeperator
      """.stripMargin
    )

    val input = createInput(file)

    input.size shouldBe 4

    input.current.toString shouldBe ""

    input.changeToNextCommand()
    input.current.toString shouldBe "ABCDEFGH"

    input.changeToNextCommand()
    input.current.toString shouldBe
      """|A
         |B
         |C
         |D""".stripMargin

    input.changeToNextCommand()
    input.current.toString shouldBe
      """|ABCDEF
         |GHIJKL""".stripMargin


    input.changeToNextCommand()
    input.current.toString shouldBe ""
  }


  it should "not write changed inputs to file which were not saved" in {
    val (contents, file) = memoryFile(
      s"""|ABCD
          |$HistorySeperator
          |EFGH
          |$HistorySeperator
          |""".stripMargin
    )

    val input = createInput(file)

    input.changeToNextCommand()
    input ++= "ABC"
    input.changeToNextCommand()
    input ++= "ABC"
    input.changeToNextCommand()
    input ++= "ABC"

    input.saveCurrent()

    input.saveToFile()

    contents.toString shouldBe
      s"""|ABCD
          |$HistorySeperator
          |EFGH
          |$HistorySeperator
          |ABC
          |$HistorySeperator
          |""".stripMargin
  }


  it should "keep the old command in history when a command has been modified and saved" in {
    val (contents, file) = memoryFile(
      s"""|ABCD
          |$HistorySeperator
          |EFGH
          |$HistorySeperator
          |""".stripMargin
    )

    val input = createInput(file)

    input.changeToNextCommand()
    input ++= "123" // Cursor is at the start of the line so the result should be 123ABCD
    input.saveCurrent()

    input.saveToFile()

    contents.toString shouldBe
      s"""|ABCD
          |$HistorySeperator
          |EFGH
          |$HistorySeperator
          |123ABCD
          |$HistorySeperator
          |""".stripMargin
  }

  it should "not keep duplicates of commands but instead move the command to the front when used again" in {
    val (contents, file) = memoryFile(
      s"""|ABCD
          |$HistorySeperator
          |EFGH
          |$HistorySeperator
          |""".stripMargin
    )

    val input = createInput(file)

    input ++= "ABCD" // Cursor is at the start of the line so the result should be 123ABCD
    input.saveCurrent()

    input.saveToFile()

    contents.toString shouldBe
      s"""|EFGH
          |$HistorySeperator
          |ABCD
          |$HistorySeperator
          |""".stripMargin
  }

  private def memoryFile(content: String = ""): (StringBuilder, File) = {
    val buffer = new StringBuilder
    val file = mock[File]
    // Save the data to a local stringBuilder instead
    file.write(*)(*, *) answers { invocation =>
      buffer.clear
      buffer ++= invocation.getArgument(0)
      file
    }

    file.exists returns true
    file.lineIterator returns content.lines

    (buffer, file)
  }

  private def createInput(file: File, maxHistorySize: Int = DefaultMaxHistory, tabSize: Int = DefaultTabSize) = {
    Input(file, mock[Clipboard], maxHistorySize, tabSize)
  }

}
