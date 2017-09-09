package tlang.repl.input

import better.files.File
import tlang.testutils.UnitSpec

// When testing the InputHistory class we should maybe mock more of it's dependencies
// such as CircularBuffer and InputBuffer but it makes writing the tests really difficult
class InputHistorySpec extends UnitSpec {

  import InputHistory._


  val DefaultMaxHistory = 25
  val TabSize           = 2


  behavior of "Input history"


  it should "create a new history file if it does not exist" in {
    val historyFile = mock[File]
    historyFile.exists returns false

    InputHistory(historyFile, 1, 1)

    there was one(historyFile).createIfNotExists()
  }

  it should "save input history to an empty file" in {
    val (fileContents, file) = memoryFile()


    val inputHistory = InputHistory(file, DefaultMaxHistory, TabSize)

    inputHistory.saveToFile()
    fileContents.toString shouldBe ""


    inputHistory.current ++= "ABC"
    inputHistory.saveCurrent()
    inputHistory.saveToFile()
    fileContents.toString shouldBe s"ABC${ Seperator }"


    inputHistory.current ++= "DEF"
    inputHistory.saveCurrent()
    inputHistory.saveToFile()
    fileContents.toString shouldBe s"ABC${ Seperator }DEF${ Seperator }"

    inputHistory.current ++= "GHI"
    inputHistory.saveCurrent()
    inputHistory.saveToFile()
    fileContents.toString shouldBe s"ABC${ Seperator }DEF${ Seperator }GHI${ Seperator }"
  }

  it should "not write the same content to file twice" in {
    val (fileContents, file) = memoryFile()


    val inputHistory = InputHistory(file, DefaultMaxHistory, TabSize)

    fileContents.toString shouldBe ""


    inputHistory.current ++= "ABC"
    inputHistory.saveCurrent()

    inputHistory.saveToFile()
    inputHistory.saveToFile()

    there was one(file).write(*)(*, *)
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

    val inputHistory = InputHistory(file, DefaultMaxHistory, TabSize)

    inputHistory.size shouldBe 4

    inputHistory.current.toString shouldBe ""

    inputHistory.goToNext()
    inputHistory.current.toString shouldBe "ABCDEFGH"

    inputHistory.goToNext()
    inputHistory.current.toString shouldBe
      """|A
         |B
         |C
         |D""".stripMargin

    inputHistory.goToNext()
    inputHistory.current.toString shouldBe
      """|ABCDEF
         |GHIJKL""".stripMargin


    inputHistory.goToNext()
    inputHistory.current.toString shouldBe ""
  }


  it should "not write changed inputs to file which were not saved" in {
    val (contents, file) = memoryFile(
      s"""|ABCD
          |$HistorySeperator
          |EFGH
          |$HistorySeperator
          |""".stripMargin
    )

    val inputHistory = InputHistory(file, DefaultMaxHistory, TabSize)

    inputHistory.goToNext()
    inputHistory.current ++= "ABC"
    inputHistory.goToNext()
    inputHistory.current ++= "ABC"
    inputHistory.goToNext()
    inputHistory.current ++= "ABC"

    inputHistory.saveCurrent()

    inputHistory.saveToFile()

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

    val inputHistory = InputHistory(file, DefaultMaxHistory, TabSize)

    inputHistory.goToNext()
    inputHistory.current ++= "123" // Cursor is at the start of the line so the result should be 123ABCD
    inputHistory.saveCurrent()

    inputHistory.saveToFile()

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

    val inputHistory = InputHistory(file, DefaultMaxHistory, TabSize)

    inputHistory.current ++= "ABCD" // Cursor is at the start of the line so the result should be 123ABCD
    inputHistory.saveCurrent()

    inputHistory.saveToFile()

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

}
