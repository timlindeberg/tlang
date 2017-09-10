package tlang.repl.input

import tlang.testutils.UnitSpec

class InputBufferSpec extends UnitSpec {


  //  it should "add one character" in {
  //    val inputBuffer = createInputBuffer()
  //
  //    inputBuffer += 'A'
  //    inputBuffer += 'B'
  //    inputBuffer += 'C'
  //    inputBuffer += 'D'
  //    inputBuffer += '\n'
  //    inputBuffer += '\t'
  //    inputBuffer += ' '
  //
  //    inputBuffer.toString shouldBe "ABCD" + System.lineSeparator + "\t "
  //  }
  //
  //
  //  it should "add multiple characters" in {
  //    val inputBuffer = createInputBuffer()
  //
  //    inputBuffer ++= "ABCD"
  //    inputBuffer ++= "\n"
  //    inputBuffer ++= "EFGH"
  //
  //    inputBuffer.toString shouldBe "ABCD\nEFGH"
  //  }
  //
  //
  //  it should "add indentation when adding a newline" in {
  //    val inputBuffer = createInputBuffer()
  //    inputBuffer ++=
  //      s"""|ABCDEF
  //          |\t\tABC""".stripMargin
  //
  //    inputBuffer += '\n'
  //    inputBuffer ++= "DEF"
  //
  //    inputBuffer.toString shouldBe
  //      s"""|ABCDEF
  //          |\t\tABC
  //          |\t\tDEF""".stripMargin
  //
  //    inputBuffer += '\n'
  //    inputBuffer += '\n'
  //    inputBuffer += '\n'
  //    inputBuffer ++= "GHI"
  //
  //    inputBuffer.toString shouldBe
  //      s"""|ABCDEF
  //          |\t\tABC
  //          |\t\tDEF
  //          |\t\t
  //          |\t\t
  //          |\t\tGHI""".stripMargin
  //  }
  //
  //
  //  it should "handle moving the cursor left and right" in {
  //    val inputBuffer = createInputBuffer()
  //    inputBuffer ++= "ABCDE"
  //
  //    inputBuffer.moveCursorLeft(1)
  //    inputBuffer.moveSecondaryCursor()
  //    inputBuffer += '1'
  //    inputBuffer.toString shouldBe "ABCD1E"
  //
  //    inputBuffer.moveCursorLeft(5)
  //    inputBuffer.moveSecondaryCursor()
  //    inputBuffer += '2'
  //    inputBuffer += '3'
  //    inputBuffer.toString shouldBe "23ABCD1E"
  //
  //    inputBuffer.moveCursorRight(1)
  //    inputBuffer.moveSecondaryCursor()
  //    inputBuffer += '4'
  //    inputBuffer.toString shouldBe "23A4BCD1E"
  //
  //    inputBuffer.moveCursorRight(3)
  //    inputBuffer.moveSecondaryCursor()
  //    inputBuffer += '5'
  //    inputBuffer.toString shouldBe "23A4BCD51E"
  //  }
  //
  //
  //  it should "handle moving the cursor up and down" in {
  //    val inputBuffer = createInputBuffer()
  //    inputBuffer ++=
  //      """|ABC
  //         |D
  //         |EFGHIJK
  //         |LMN
  //         |O
  //         |PQRS
  //         |
  //         |TUV""".stripMargin
  //
  //
  //  }


  it should "handle cursor navigation with large movements" in { pending }
  it should "handle secondary cursor navigation" in { pending }


  it should "remove the selected text when adding a character" in {

  }

  it should "handle removing a character at the cursor position" in { pending }
  it should "handle removing multiple characters between the cursor positions" in { pending }

  it should "handle copying the selected characters" in { pending }
  it should "handle copying the current line" in { pending }

  it should "handle pasting at the cursor position" in { pending }
  it should "remove the selected text pasting" in { pending }

  it should "handle cutting the selected characters" in { pending }
  it should "handle cutting the current line" in { pending }

  it should "handle removing with large movement" in { pending }

  it should "handle undo" in { pending }
  it should "handle redo" in { pending }


  private def createInputBuffer() = {
    InputBuffer()
  }

}
