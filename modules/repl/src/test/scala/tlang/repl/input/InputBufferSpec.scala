package tlang.repl.input

import tlang.testutils.UnitSpec
import tlang.utils.Extensions._
import tlang.utils.Position

class InputBufferSpec extends UnitSpec {


  val TabWidth = 4

  behavior of "An inputbuffer"

  it should "add one character" in {
    InputBuffer(TabWidth)
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
    InputBuffer(TabWidth)
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
         |LMN""".stripMargin, TabWidth)
      .add('1')
      .toString shouldBe
      """|1ABC
         |D
         |EFGHIJK
         |LMN""".stripMargin
  }

  it should "handle moving the cursor left and right" in {
    InputBuffer(TabWidth)
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
    InputBuffer(TabWidth)
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
    InputBuffer(TabWidth)
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
    InputBuffer(TabWidth)
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
    InputBuffer(TabWidth)
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
    InputBuffer(TabWidth)
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
    InputBuffer(TabWidth)
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
         |LMN""".stripMargin, TabWidth)
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
         |LMN""".stripMargin, TabWidth)
      .moveCursor(6)
      .add('1')
      .moveCursor(5, 2)
      .add('2')
      .moveCursor(2, 3, moveSecondary = false)
      .removeSelected()
      .moveCursor(5, 1)
      .add('3')
      .toString shouldBe
      """|ABC
         |D3
         |1EFGH2N""".stripMargin
  }

  it should "select the current line" in {
    InputBuffer(
      """|AB CDE FG
         |HIJKL MNOPQR
         |STU""".stripMargin, TabWidth)
      .selectCurrentLine()
      .use { buffer =>
        buffer.selected shouldBe s"AB CDE FG"

        buffer.secondaryCursor.xy shouldBe(0, 0)
        buffer.mainCursor.xy shouldBe(9, 0)
      }
      .moveCursorVertical(-1)
      .selectCurrentLine(selectNewLine = true)
      .use { buffer =>
        buffer.selected shouldBe s"HIJKL MNOPQR$NL"

        buffer.secondaryCursor.xy shouldBe(0, 1)
        buffer.mainCursor.xy shouldBe(0, 2)
      }
      .selectCurrentLine()
      .use { buffer =>
        buffer.selected shouldBe "STU"

        buffer.secondaryCursor.xy shouldBe(0, 2)
        buffer.mainCursor.xy shouldBe(3, 2)
      }
  }

  it should "select the current word" in {
    InputBuffer(
      """|AB CDE FG
         |HIJ   MNOPQR
         |STU(abc)""".stripMargin, TabWidth)
      .selectCurrentWord()
      .use { buffer =>
        buffer.selected shouldBe s"AB"

        buffer.secondaryCursor.xy shouldBe(0, 0)
        buffer.mainCursor.xy shouldBe(2, 0)
      }
      .moveCursorHorizontal(1)
      .selectCurrentWord()
      .use { buffer =>
        buffer.selected shouldBe s"CDE"

        buffer.secondaryCursor.xy shouldBe(3, 0)
        buffer.mainCursor.xy shouldBe(6, 0)
      }
      .selectCurrentWord()
      .use { buffer =>
        buffer.selected shouldBe s"CDE"

        buffer.secondaryCursor.xy shouldBe(3, 0)
        buffer.mainCursor.xy shouldBe(6, 0)
      }
      .moveCursorHorizontal(-1)
      .selectCurrentWord()
      .use { buffer =>
        buffer.selected shouldBe s"CDE"

        buffer.secondaryCursor.xy shouldBe(3, 0)
        buffer.mainCursor.xy shouldBe(6, 0)
      }
      .moveCursorHorizontal(-2)
      .selectCurrentWord()
      .use { buffer =>
        buffer.selected shouldBe s"CDE"

        buffer.secondaryCursor.xy shouldBe(3, 0)
        buffer.mainCursor.xy shouldBe(6, 0)
      }
      .moveCursorVertical(-1)
      .selectCurrentWord()
      .use { buffer =>
        buffer.selected shouldBe "MNOPQR"

        buffer.secondaryCursor.xy shouldBe(6, 1)
        buffer.mainCursor.xy shouldBe(12, 1)
      }
      .moveCursorHorizontal(-8)
      .selectCurrentWord()
      .use { buffer =>
        buffer.selected shouldBe ""

        buffer.secondaryCursor.xy shouldBe(4, 1)
        buffer.mainCursor.xy shouldBe(4, 1)
      }
      .moveCursorVertical(-1)
      .selectCurrentWord()
      .use { buffer =>
        buffer.selected shouldBe "abc"

        buffer.secondaryCursor.xy shouldBe(4, 2)
        buffer.mainCursor.xy shouldBe(7, 2)
      }

  }

  it should "compensate for tab characters" in {
    test("Tab width 4") {
      InputBuffer(
        s"""|\tABCDEFGHIJ
            |\t\t\tKL""".stripMargin, TabWidth)

        // Cursor before tab
        .moveCursorVertical(-1)
        .use { _.mainCursor.xy shouldBe(0, 1) }
        .moveCursorVertical(1)
        .use { _.mainCursor.xy shouldBe(0, 0) }

        // Cursor before A
        .moveCursorHorizontal(1)
        .moveCursorVertical(-1)
        .use { _.mainCursor.xy shouldBe(4, 1) }
        .moveCursorVertical(1)
        .use { _.mainCursor.xy shouldBe(4, 0) }

        // Cursor before B
        .moveCursorHorizontal(1)
        .moveCursorVertical(-1)
        .use { _.mainCursor.xy shouldBe(4, 1) }
        .moveCursorVertical(1)
        .use { _.mainCursor.xy shouldBe(5, 0) }

        // Cursor before C
        .moveCursorHorizontal(1)
        .moveCursorVertical(-1)
        .use { _.mainCursor.xy shouldBe(8, 1) }
        .moveCursorVertical(1)
        .use { _.mainCursor.xy shouldBe(6, 0) }

        // Cursor before D
        .moveCursorHorizontal(1)
        .moveCursorVertical(-1)
        .use { _.mainCursor.xy shouldBe(8, 1) }
        .moveCursorVertical(1)
        .use { _.mainCursor.xy shouldBe(7, 0) }

        // Cursor before E
        .moveCursorHorizontal(1)
        .moveCursorVertical(-1)
        .use { _.mainCursor.xy shouldBe(8, 1) }
        .moveCursorVertical(1)
        .use { _.mainCursor.xy shouldBe(8, 0) }

        // Cursor before F
        .moveCursorHorizontal(1)
        .moveCursorVertical(-1)
        .use { _.mainCursor.xy shouldBe(8, 1) }
        .moveCursorVertical(1)
        .use { _.mainCursor.xy shouldBe(9, 0) }

        // Cursor before G
        .moveCursorHorizontal(1)
        .moveCursorVertical(-1)
        .use { _.mainCursor.xy shouldBe(12, 1) }
        .moveCursorVertical(1)
        .use { _.mainCursor.xy shouldBe(10, 0) }

        // Cursor before H
        .moveCursorHorizontal(1)
        .moveCursorVertical(-1)
        .use { _.mainCursor.xy shouldBe(12, 1) }
        .moveCursorVertical(1)
        .use { _.mainCursor.xy shouldBe(11, 0) }

        // Cursor before I
        .moveCursorHorizontal(1)
        .moveCursorVertical(-1)
        .use { _.mainCursor.xy shouldBe(12, 1) }
        .moveCursorVertical(1)
        .use { _.mainCursor.xy shouldBe(12, 0) }

        // Cursor before J
        .moveCursorHorizontal(1)
        .moveCursorVertical(-1)
        .use { _.mainCursor.xy shouldBe(13, 1) }
        .moveCursorVertical(1)
        .use { _.mainCursor.xy shouldBe(13, 0) }

        // Cursor after J
        .moveCursorHorizontal(1)
        .moveCursorVertical(-1)
        .use { _.mainCursor.xy shouldBe(14, 1) }
        .moveCursorVertical(1)
        .use { _.mainCursor.xy shouldBe(14, 0) }
    }

    test("Tab width 3") {
      InputBuffer(
        s"""|\tABCDEFGHIJ
            |\t\t\tKL""".stripMargin, tabWidth = 3)

        // Cursor before tab
        .moveCursorVertical(-1)
        .use { _.mainCursor.xy shouldBe(0, 1) }
        .moveCursorVertical(1)
        .use { _.mainCursor.xy shouldBe(0, 0) }

        // Cursor before A
        .moveCursorHorizontal(1)
        .moveCursorVertical(-1)
        .use { _.mainCursor.xy shouldBe(3, 1) }
        .moveCursorVertical(1)
        .use { _.mainCursor.xy shouldBe(3, 0) }

        // Cursor before B
        .moveCursorHorizontal(1)
        .moveCursorVertical(-1)
        .use { _.mainCursor.xy shouldBe(3, 1) }
        .moveCursorVertical(1)
        .use { _.mainCursor.xy shouldBe(4, 0) }

        // Cursor before C
        .moveCursorHorizontal(1)
        .moveCursorVertical(-1)
        .use { _.mainCursor.xy shouldBe(6, 1) }
        .moveCursorVertical(1)
        .use { _.mainCursor.xy shouldBe(5, 0) }

        // Cursor before D
        .moveCursorHorizontal(1)
        .moveCursorVertical(-1)
        .use { _.mainCursor.xy shouldBe(6, 1) }
        .moveCursorVertical(1)
        .use { _.mainCursor.xy shouldBe(6, 0) }

        // Cursor before E
        .moveCursorHorizontal(1)
        .moveCursorVertical(-1)
        .use { _.mainCursor.xy shouldBe(6, 1) }
        .moveCursorVertical(1)
        .use { _.mainCursor.xy shouldBe(7, 0) }

        // Cursor before F
        .moveCursorHorizontal(1)
        .moveCursorVertical(-1)
        .use { _.mainCursor.xy shouldBe(9, 1) }
        .moveCursorVertical(1)
        .use { _.mainCursor.xy shouldBe(8, 0) }

        // Cursor before G
        .moveCursorHorizontal(1)
        .moveCursorVertical(-1)
        .use { _.mainCursor.xy shouldBe(9, 1) }
        .moveCursorVertical(1)
        .use { _.mainCursor.xy shouldBe(9, 0) }

        // Cursor before H
        .moveCursorHorizontal(1)
        .moveCursorVertical(-1)
        .use { _.mainCursor.xy shouldBe(10, 1) }
        .moveCursorVertical(1)
        .use { _.mainCursor.xy shouldBe(10, 0) }

        // Cursor before I
        .moveCursorHorizontal(1)
        .moveCursorVertical(-1)
        .use { _.mainCursor.xy shouldBe(11, 1) }
        .moveCursorVertical(1)
        .use { _.mainCursor.xy shouldBe(11, 0) }

        // Cursor before J
        .moveCursorHorizontal(1)
        .moveCursorVertical(-1)
        .use { _.mainCursor.xy shouldBe(11, 1) }
        .moveCursorVertical(1)
        .use { _.mainCursor.xy shouldBe(12, 0) }

        // Cursor after J
        .moveCursorHorizontal(1)
        .moveCursorVertical(-1)
        .use { _.mainCursor.xy shouldBe(11, 1) }
        .moveCursorVertical(1)
        .use { _.mainCursor.xy shouldBe(13, 0) }
    }
  }

}
