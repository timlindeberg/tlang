package tlang.repl.input

import tlang.testutils.UnitSpec
import tlang.utils.Extensions._
import tlang.utils.Position

class InputBufferSpec extends UnitSpec {


  behavior of "An inputbuffer"

  it should "add one character" in {
    InputBuffer.Empty
      .add('A')
      .add('B')
      .add('C')
      .add('D')
      .add('\n')
      .add('\t')
      .add(' ')
      .toString shouldBe s"ABCD${ NL }\t "
  }


  it should "add multiple characters" in {
    InputBuffer.Empty
      .add("ABCD")
      .add(NL)
      .add("EFGH")
      .toString shouldBe s"ABCD${ NL }EFGH"
  }


  it should "be constructed from a string" in {
    InputBuffer(
      """|ABC
         |D
         |EFGHIJK
         |LMN""".stripMargin)
      .add('1')
      .toString shouldBe
      """|1ABC
         |D
         |EFGHIJK
         |LMN""".stripMargin
  }

  it should "handle moving the cursor left and right" in {
    InputBuffer.Empty
      .add("ABCDE")
      .moveCursorHorizontal(-1)
      .add('1')
      .use {
        _.toString shouldBe "ABCD1E"
      }
      .moveCursorHorizontal(-5)
      .add('2')
      .add('3')
      .use {
        _.toString shouldBe "23ABCD1E"
      }
      .moveCursorHorizontal(1)
      .add('4')
      .use {
        _.toString shouldBe "23A4BCD1E"
      }
      .moveCursorHorizontal(3)
      .add('5')
      .moveCursorHorizontal(25)
      .add('6')
      .moveCursorHorizontal(1)
      .add('7')
      .moveCursorHorizontal(-25)
      .add('8')
      .moveCursorHorizontal(-2)
      .add('9')
      .toString shouldBe "9823A4BCD51E67"
  }


  it should "handle moving the cursor up and down" in {
    InputBuffer.Empty
      .add(
        """|ABC
           |D
           |EFGHIJK
           |LMN
           |O
           |PQRS
           |
           |TUV""".stripMargin
      )
      .moveCursorVertical(2)
      .add('1')
      .use {
        _.currentLine shouldBe "PQR1S"
      }
      .moveCursorHorizontal(-2)
      .moveCursorVertical(1)
      .moveCursorVertical(1)
      .add('2')
      .use {
        _.currentLine shouldBe "LM2N"
      }
      .moveCursorVertical(1)
      .moveCursorHorizontal(4)
      .add('3')
      .moveCursorVertical(2)
      .add('4')
      .use {
        _.currentLine shouldBe "ABC4"
      }
      .moveCursorHorizontal(2)
      .add('5')
      .use {
        _.currentLine shouldBe "D5"
      }
      .toString shouldBe
      """|ABC4
         |D5
         |EFGHIJK3
         |LM2N
         |O
         |PQR1S
         |
         |TUV""".stripMargin
  }


  it should "handle cursor navigation with large movements" in {
    InputBuffer.Empty
      .add("abc \tdefgh    ijk lmn")
      .moveCursorToRightWord() // Shouldn't move the cursor
      .moveCursorToLeftWord()
      .add('1')
      .moveCursorToLeftWord()
      .moveCursorToLeftWord()
      .add('2')
      .moveCursorToLeftWord()
      .add('3')
      .moveCursorToLeftWord()
      .moveCursorToLeftWord()
      .add('4')
      .moveCursorToLeftWord()
      .add('5')
      .moveCursorToLeftWord()
      .moveCursorToLeftWord()
      .add('6')
      .moveCursorToLeftWord()
      .moveCursorToLeftWord() // Shouldn't move the cursor
      .add('7')
      .moveCursorToRightWord()
      .add('8')
      .moveCursorToRightWord()
      .add('9')
      .moveCursorToRightWord()
      .add('A')
      .moveCursorToRightWord()
      .add('B')
      .moveCursorToRightWord()
      .add('C')
      .moveCursorToRightWord()
      .add('D')
      .moveCursorToRightWord()
      .add('E')
      .moveCursorToRightWord() // Shouldn't move the cursor
      .toString shouldBe "7abc68 \t95defgh4A    B3ijk2C D1lmnE"
  }


  it should "handle secondary cursor navigation" in {
    InputBuffer.Empty
      .add(
        """|ABC
           |D
           |EFGHIJK
           |LMN
           |O
           |PQRS
           |
           |TUV""".stripMargin
      )
      .moveCursorHorizontal(-2, moveSecondary = false)
      .use { buffer =>
        buffer.selectedPosition shouldBe Position(8, 2, 8, 4)
        buffer.selected shouldBe s"UV"
      }
      .moveCursorVertical(2, moveSecondary = false)
      .use { buffer =>
        buffer.selectedPosition shouldBe Position(6, 2, 8, 4)
        buffer.selected shouldBe s"QRS${ NL }${ NL }TUV"
      }
      .moveCursorHorizontal(-1, moveSecondary = false)
      .use { buffer =>
        buffer.selectedPosition shouldBe Position(6, 1, 8, 4)
        buffer.selected shouldBe s"PQRS${ NL }${ NL }TUV"
      }
      .moveCursorVertical(3, moveSecondary = false)
      .use { buffer =>
        buffer.selectedPosition shouldBe Position(3, 1, 8, 4)
        buffer.selected shouldBe s"EFGHIJK${ NL }LMN${ NL }O${ NL }PQRS${ NL }${ NL }TUV"
      }
      .moveCursorHorizontal(4, moveSecondary = false)
      .use { buffer =>
        buffer.selectedPosition shouldBe Position(3, 5, 8, 4)
        buffer.selected shouldBe s"IJK${ NL }LMN${ NL }O${ NL }PQRS${ NL }${ NL }TUV"
      }
      .moveCursorHorizontal(2)
      .use { buffer =>
        buffer.selectedPosition shouldBe Position(3, 7, 3, 7)
        buffer.selected shouldBe ""
      }
      .moveCursorVertical(-3, moveSecondary = false)
      .use { buffer =>
        buffer.selectedPosition shouldBe Position(3, 7, 6, 5)
        buffer.selected shouldBe s"K${ NL }LMN${ NL }O${ NL }PQRS"
      }
  }


  it should "remove the selected text when adding a character" in {
    InputBuffer.Empty
      .add(
        """|ABC
           |D
           |EFGHIJK
           |LMN""".stripMargin
      )
      .moveCursorVertical(1, moveSecondary = false)
      .add('1')
      .toString shouldBe
      """|ABC
         |D
         |EFG1""".stripMargin
  }


  it should "handle adding a character to the end, start and middle of a buffer" in {
    InputBuffer.Empty
      .add(
        """|ABC
           |D
           |EFGHIJK
           |LMN""".stripMargin
      )
      .add('1')
      .moveCursorVertical(3)
      .moveCursorHorizontal(-3)
      .add('2')
      .moveCursorVertical(-1)
      .add('3')
      .toString shouldBe
      """|2ABC
         |D3
         |EFGHIJK
         |LMN1""".stripMargin
  }


  it should "handle removing a character at the cursor position" in {
    InputBuffer.Empty
      .add(
        """|ABC
           |D
           |EFGHIJK
           |LMN""".stripMargin
      )
      .removeSelected()
      .removeSelected()
      .moveCursorVertical(1)
      .removeSelected()
      .removeSelected()
      .moveCursorVertical(1)
      .removeSelected()
      .removeSelected()
      .moveCursorVertical(-1)
      .moveCursorHorizontal(4)
      .removeSelected()
      .removeSelected()
      .toString shouldBe
      """|BC
         |DFIJK
         |L""".stripMargin
  }


  it should "handle removing multiple characters between the cursor positions" in {
    InputBuffer(
      """|ABC
         |D
         |EFGHIJK
         |LMN""".stripMargin
    )
      .moveCursorHorizontal(1, moveSecondary = false)
      .removeSelected()
      .moveCursorVertical(-2, moveSecondary = false)
      .removeSelected()
      .moveCursorHorizontal(3)
      .moveCursorHorizontal(3, moveSecondary = false)
      .removeSelected()
      .removeSelected()
      .removeSelected()
      .toString shouldBe
      """|EK
         |LMN""".stripMargin
  }

  it should "handle moving the cursor to absolute positions" in {
    InputBuffer(
      """|ABC
         |D
         |EFGHIJK
         |LMN""".stripMargin
    )
      .moveCursor(6).print(_.debugString)
      .add('1').print(_.debugString)
      .moveCursor(5, 2).print(_.debugString)
      .add('2').print(_.debugString)
      .moveCursor(2, 3, moveSecondary = false).print(_.debugString)
      .removeSelected().print(_.debugString)
      .moveCursor(5, 1).print(_.debugString)
      .add('3')
      .toString shouldBe
      """|ABC
         |D3
         |1EFGH2N""".stripMargin
  }


}
