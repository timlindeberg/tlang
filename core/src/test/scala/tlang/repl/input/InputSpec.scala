package tlang.repl.input

import better.files.File
import tlang.testutils.UnitSpec
import tlang.utils.Extensions._

// When testing the InputHistory class we should maybe mock more of it's dependencies
// such as CircularBuffer and InputBuffer but it makes writing meaningful tests really difficult
class InputSpec extends UnitSpec {

  import Input._


  val DefaultMaxHistory = 25


  behavior of "Input"


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
    input.saveCurrentCommand()
    input.saveToFile()
    fileContents.toString shouldBe s"ABC${ Seperator }"


    input ++= "DEF"
    input.saveCurrentCommand()
    input.saveToFile()
    fileContents.toString shouldBe s"ABC${ Seperator }DEF${ Seperator }"

    input ++= "GHI"
    input.saveCurrentCommand()
    input.saveToFile()
    fileContents.toString shouldBe s"ABC${ Seperator }DEF${ Seperator }GHI${ Seperator }"
  }

  it should "not write the same content to file twice" in {
    val (fileContents, file) = memoryFile()


    val input = createInput(file)

    fileContents.toString shouldBe ""


    input ++= "ABC"
    input.saveCurrentCommand()

    input.saveToFile()
    input.saveToFile()

    there was one(file).write(*)(*, *)
  }


  it should "not write to file unless changes have been saved" in {
    val (_, file) = memoryFile()


    val input = createInput(file)

    input ++= "ABC"
    input.saveToFile()

    there was no(file).write(*)(*, *)
  }


  it should "change input buffer when going up/down when at the end of a command" in {
    val (_, file) = memoryFile(
      s"""|ABCD
          |$HistorySeperator
          |EFGH
          |$HistorySeperator
          |IJKL
          |$HistorySeperator
          |""".stripMargin
    )

    val input = createInput(file)
    input.toString shouldBe ""

    input.up()
    input.toString shouldBe "IJKL"

    input.up()
    input.toString shouldBe "EFGH"

    input.up()
    input.toString shouldBe "ABCD"

    input.up()
    input.toString shouldBe ""

    input.down()
    input.toString shouldBe "ABCD"
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

    input.toString shouldBe ""

    input.down()
    input.toString shouldBe "ABCDEFGH"

    input.down()
    input.toString shouldBe
      """|A
         |B
         |C
         |D""".stripMargin

    input.down().down().down().down()
    input.toString shouldBe
      """|ABCDEF
         |GHIJKL""".stripMargin


    input.down().down()
    input.toString shouldBe ""
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

    input.down()
    input ++= "ABC"
    input.down()
    input ++= "ABC"
    input.down()
    input ++= "ABC"

    input.saveCurrentCommand()

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

    input.down()
    input ++= "123" // Cursor is at the start of the line so the result should be 123ABCD
    input.saveCurrentCommand()

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
    input.saveCurrentCommand()

    input.saveToFile()

    contents.toString shouldBe
      s"""|EFGH
          |$HistorySeperator
          |ABCD
          |$HistorySeperator
          |""".stripMargin
  }

  it should "undo and redo changes to the current command" in {
    val input = createInput()

    input ++= "ABC"
    input += 'D'
    input += 'E'
    input ++= "FGH"
    input ++= "IJKLMN"

    input.toString shouldBe "ABCDEFGHIJKLMN"

    input.undo() shouldBe true
    input.toString shouldBe "ABCDEFGH"

    input.undo() shouldBe true
    input.toString shouldBe "ABCDE"

    input.undo() shouldBe true
    input.toString shouldBe "ABCD"

    input.undo() shouldBe true
    input.toString shouldBe "ABC"

    input.undo() shouldBe true
    input.toString shouldBe ""

    input.undo() shouldBe false
    input.toString shouldBe ""

    input.redo() shouldBe true
    input.toString shouldBe "ABC"

    input.redo() shouldBe true
    input.toString shouldBe "ABCD"

    input ++= "EFG"
    input.toString shouldBe "ABCDEFG"

    input.redo() shouldBe false
    input.toString shouldBe "ABCDEFG"

    input.undo() shouldBe true
    input.toString shouldBe "ABCD"

    input.redo() shouldBe true
    input.toString shouldBe "ABCDEFG"

    input.redo() shouldBe false
  }

  it should "only be able to undo certain actions" in {
    val clipboard = mock[Clipboard]

    clipboard.content returns "Pasted String"

    val input = createInput(clipboard = clipboard)

    // Only actions which affect the buffer contents can be undone,
    // not actions that only move the cursor
    input
      .add("ABC") //             can be undone
      .add('D') //               can be undone
      .removeSelected() //       can be undone
      .paste() //                can be undone
      .left(shiftDown = true) // can NOT be undone
      .copySelected() //         can NOT be undone
      .left(shiftDown = true) // can NOT be undone
      .cutSelected() //          can be undone
      .removeToLeftWord() //     can be undone
      .right() //                can NOT be undone
      .left(altDown = true) //   can NOT be undone
      .right(altDown = true) //  can NOT be undone
      .up() //                   can NOT be undone
      .down() //                 can NOT be undone

    input.toString shouldBe "ABCPasted "

    input.undo() shouldBe true
    input.toString shouldBe "ABCPasted Stri"

    input.undo() shouldBe true
    input.toString shouldBe "ABCPasted String"

    input.undo() shouldBe true
    input.toString shouldBe "ABC"

    input.undo() shouldBe true
    input.toString shouldBe "ABCD"

    input.undo() shouldBe true
    input.toString shouldBe "ABC"

    input.undo() shouldBe true
    input.toString shouldBe ""

    input.undo() shouldBe false
  }


  it should "add indentation when adding a newline character if the current line is indented" in {
    val input = createInput()
    input
      .add(
        s"""|ABCDEF
            |\t\tABC""".stripMargin
      )
      .add('\n')
      .add("DEF")
      .use {
        _.toString shouldBe
          s"""|ABCDEF
              |\t\tABC
              |\t\tDEF""".stripMargin
      }
      .add('\n')
      .add('\n')
      .add('\n')
      .add("GHI")
      .use {
        _.toString shouldBe
          s"""|ABCDEF
              |\t\tABC
              |\t\tDEF
              |\t\t
              |\t\t
              |\t\tGHI""".stripMargin
      }
  }

  it should "remove to next word" in {
    val input = createInput()
    input
      .add("ABC  DEF   GHI   \t\t  JKLM  \t")
      .removeToLeftWord()
      .use {
        _.toString shouldBe "ABC  DEF   GHI   \t\t  JKLM"
      }
      .removeToLeftWord()
      .use {
        _.toString shouldBe "ABC  DEF   GHI   \t\t  "
      }
      .removeToLeftWord()
      .use {
        _.toString shouldBe "ABC  DEF   GHI"
      }
      .removeToLeftWord()
      .use {
        _.toString shouldBe "ABC  DEF   "
      }
      .removeToLeftWord()
      .use {
        _.toString shouldBe "ABC  DEF"
      }
      .removeToLeftWord()
      .use {
        _.toString shouldBe "ABC  "
      }
      .removeToLeftWord()
      .use {
        _.toString shouldBe "ABC"
      }
      .removeToLeftWord()
      .use {
        _.toString shouldBe ""
      }
      .removeToLeftWord()
      .use {
        _.toString shouldBe ""
      }
  }


  it should "handle pasting at the cursor position" in {
    val clipboard = mock[Clipboard]
    clipboard.content returns s"Pasted${ NL }lines"
    val input = createInput(clipboard = clipboard)

    input
      .add(
        """|ABC
           |D
           |EFGHIJK
           |LMN""".stripMargin
      )
      .up()
      .right()
      .paste()
      .toString shouldBe
      """|ABC
         |D
         |EFGHPasted
         |linesIJK
         |LMN""".stripMargin
  }


  it should "remove the selected text when pasting" in {
    val clipboard = mock[Clipboard]
    clipboard.content returns s"Pasted line"
    val input = createInput(clipboard = clipboard)

    input
      .add(
        """|ABC
           |D
           |EFGHIJK
           |LMN""".stripMargin
      )
      .up(shiftDown = true).up(shiftDown = true)
      .paste()
      .toString shouldBe
      """|ABC
         |DPasted line""".stripMargin
  }


  it should "copy selected characters" in {
    val clipboard = mock[Clipboard]
    val input = createInput(clipboard = clipboard)

    input
      .add(
        """|ABC
           |D
           |EFGHIJK
           |LMN""".stripMargin
      )
      .up(shiftDown = true)
      .copySelected()
      .left().left()
      .right(shiftDown = true).right(shiftDown = true).right(shiftDown = true).right(shiftDown = true)
      .copySelected()

    there was one(clipboard).setContent(s"HIJK${ NL }LMN")
    there was one(clipboard).setContent(s"FGHI")
  }


  it should "copy the current line when nothing is selected" in {
    val clipboard = mock[Clipboard]
    val input = createInput(clipboard = clipboard)

    input
      .add(
        """|ABC
           |D
           |EFGHIJK
           |LMN""".stripMargin
      )
      .up()
      .copySelected()
      .up().up()
      .copySelected()
      .up().up()
      .copySelected()

    there was one(clipboard).setContent(s"EFGHIJK${ NL }")
    there was one(clipboard).setContent(s"D${ NL }")
    there was one(clipboard).setContent(s"ABC${ NL }")
  }

  it should "move the cursor to the start or end of the current line" in {
    createInput()
      .add(
        """|ABC
           |D
           |EFGHIJK
           |LMN""".stripMargin
      )
      .up()
      .moveCursorToEndOfLine()
      .add('1')
      .left()
      .moveCursorToStartOfLine(shiftDown = true)
      .removeSelected()
      .up()
      .moveCursorToEndOfLine()
      .add('2')
      .up()
      .moveCursorToStartOfLine()
      .add('3')
      .toString shouldBe
      """|3ABC
         |D2
         |1
         |LMN""".stripMargin
  }

  it should "remove to the start of the current line" in {
    createInput()
      .add(
        """|ABC
           |D
           |EFGHIJK
           |LMN""".stripMargin
      )
      .up()
      .removeToStartOfLine()
      .add('1')
      .right()
      .right()
      .removeToStartOfLine()
      .up()
      .up()
      .right()
      .right()
      .right()
      .removeToStartOfLine()
      .toString shouldBe
      """|
         |D
         |JK
         |LMN""".stripMargin
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

  private def createInput(file: File = memoryFile()._2, clipboard: Clipboard = mock[Clipboard], maxHistorySize: Int = DefaultMaxHistory) = {
    Input(file, clipboard, maxHistorySize)
  }

}
