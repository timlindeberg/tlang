package tlang.repl

import com.googlecode.lanterna.TerminalPosition
import com.googlecode.lanterna.input.{MouseAction, MouseActionType}
import com.googlecode.lanterna.terminal.Terminal
import tlang.formatting.grid.Grid
import tlang.repl.terminal.{MouseClick, MouseDrag, ReplTerminal}
import tlang.testutils.UnitSpec

class ReplTerminalSpec extends UnitSpec {

  behavior of "A terminal"


  val DefaultDoubleClickTime = 0


  it should "output boxes" in {
    val underlyingTerminal = mock[Terminal]
    underlyingTerminal.getCursorPosition returns(
      new TerminalPosition(0, 0),
      new TerminalPosition(0, 6),
      new TerminalPosition(0, 12)
    )

    val terminal = ReplTerminal(underlyingTerminal, DefaultDoubleClickTime, createMockFormatter(width = 20).formatting)
    val grid = mock[Grid]
    grid.render() returns
      """|╒══════════════════╕
         |│       ABC        │
         |╞══════════════════╡
         |│                  │
         |│                  │
         |└──────────────────┘""".stripMargin

    terminal.putBox(grid, resetStartPosition = false)
    terminal.boxStartPosition shouldBe new TerminalPosition(0, 6)
    terminal.boxHeight shouldBe 6


    terminal.putBox(grid, resetStartPosition = false)
    terminal.boxStartPosition shouldBe new TerminalPosition(0, 12)
    terminal.boxHeight shouldBe 6
  }


  it should "create correct mouse click events" in {

    test("size 20") {
      val underlyingTerminal = mock[Terminal]
      underlyingTerminal.getCursorPosition returns new TerminalPosition(0, 8)
      val terminal = ReplTerminal(underlyingTerminal, DefaultDoubleClickTime, createMockFormatter(width = 20).formatting)
      val grid = mock[Grid]
      grid.render() returns
        """|╒══════════════════╕
           |│       ABC        │
           |╞══════════════════╡
           |│ 1                │
           |│                  │
           |│         2        │
           |│                  │
           |└──────────────────┘""".stripMargin

      terminal.putBox(grid, resetStartPosition = true)

      underlyingTerminal.readInput() returns(
        mouseClick(19, 0), // outside the buffer
        mouseClick(2, 3),
        mouseClick(11, 5)
      )

      terminal.readInput() shouldBe MouseClick(0, 0, 1)
      terminal.readInput() shouldBe MouseClick(9, 2, 1)

    }

    test("size 25") {
      val underlyingTerminal = mock[Terminal]
      underlyingTerminal.getCursorPosition returns new TerminalPosition(0, 10)

      val terminal = ReplTerminal(underlyingTerminal, DefaultDoubleClickTime, createMockFormatter(width = 25).formatting)
      val grid = mock[Grid]
      grid.render() returns
        """|╒═══════════════════════╕
           |│          ABC          │
           |╞═══════════════════════╡
           |│                     1 │
           |│                       │
           |│                       │
           |│                       │
           |│                2      │
           |│                     3 │
           |└───────────────────────┘""".stripMargin


      terminal.putBox(grid, resetStartPosition = true)

      underlyingTerminal.readInput() returns(
        mouseClick(0, 0), // outside the buffer
        mouseClick(23, 3),
        mouseClick(3, 2), // outside the buffer
        mouseClick(7, 9), // outside the buffer
        mouseClick(18, 7),
        mouseClick(23, 8)
      )


      terminal.readInput() shouldBe MouseClick(21, 0, 1)
      terminal.readInput() shouldBe MouseClick(16, 4, 1)
      terminal.readInput() shouldBe MouseClick(21, 5, 1)

    }

  }


  it should "create correct mouse dobule and triple click events" in {
    val underlyingTerminal = mock[Terminal]
    underlyingTerminal.getCursorPosition returns new TerminalPosition(0, 8)

    val doubleClickTime = 5

    val terminal = ReplTerminal(underlyingTerminal, doubleClickTime, createMockFormatter(width = 20).formatting)
    val grid = mock[Grid]
    grid.render() returns
      """|╒══════════════════╕
         |│       ABC        │
         |╞══════════════════╡
         |│ 1                │
         |│                  │
         |│         2        │
         |│                  │
         |└──────────────────┘""".stripMargin

    terminal.putBox(grid, resetStartPosition = true)

    underlyingTerminal.readInput() returns mouseClick(2, 3)

    terminal.readInput() shouldBe MouseClick(0, 0, 1)
    terminal.readInput() shouldBe MouseClick(0, 0, 2)

    Thread.sleep(doubleClickTime)

    terminal.readInput() shouldBe MouseClick(0, 0, 1)
    terminal.readInput() shouldBe MouseClick(0, 0, 2)
    terminal.readInput() shouldBe MouseClick(0, 0, 3)

    Thread.sleep(doubleClickTime)

    terminal.readInput() shouldBe MouseClick(0, 0, 1)
  }


  it should "create correct drag events" in {
    val underlyingTerminal = mock[Terminal]
    underlyingTerminal.getCursorPosition returns new TerminalPosition(0, 10)

    val terminal = ReplTerminal(underlyingTerminal, DefaultDoubleClickTime, createMockFormatter(width = 25).formatting)
    val grid = mock[Grid]
    grid.render() returns
      """|╒═══════════════════════╕
         |│          ABC          │
         |╞═══════════════════════╡
         |│                     1 │
         |│                       │
         |│                       │
         |│                       │
         |│                2      │
         |│                     3 │
         |└───────────────────────┘""".stripMargin


    terminal.putBox(grid, resetStartPosition = true)

    underlyingTerminal.readInput() returns(
      mouseDrag(0, 0), // outside the buffer
      mouseDrag(23, 3),
      mouseDrag(3, 2), // outside the buffer
      mouseDrag(7, 9), // outside the buffer
      mouseDrag(18, 7),
      mouseDrag(23, 8),
      mouseDrag(26, 6), // outside the buffer
      mouseDrag(100, 100) // outside the buffer
    )


    terminal.readInput() shouldBe MouseDrag(0, 0)
    terminal.readInput() shouldBe MouseDrag(21, 0)
    terminal.readInput() shouldBe MouseDrag(1, 0)
    terminal.readInput() shouldBe MouseDrag(5, 5)
    terminal.readInput() shouldBe MouseDrag(16, 4)
    terminal.readInput() shouldBe MouseDrag(21, 5)
    terminal.readInput() shouldBe MouseDrag(21, 3)
    terminal.readInput() shouldBe MouseDrag(21, 5)

  }

  private def mouseClick(x: Int, y: Int) = new MouseAction(MouseActionType.CLICK_DOWN, 1, new TerminalPosition(x, y))
  private def mouseDrag(x: Int, y: Int) = new MouseAction(MouseActionType.DRAG, 1, new TerminalPosition(x, y))


}
