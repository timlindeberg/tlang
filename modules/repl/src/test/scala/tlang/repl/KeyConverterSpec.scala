package tlang.repl

import com.googlecode.lanterna.TerminalPosition
import com.googlecode.lanterna.input.{KeyStroke, KeyType, MouseAction, MouseActionType}
import tlang.repl.terminal._
import tlang.testutils.UnitSpec

class KeyConverterSpec extends UnitSpec {

  val DefaultDoubleClickTime = 0

  behavior of "A key converter"

  it should "create correct mouse click events" in {

    test("size 20x8") {
      val keyConverter = KeyConverter(doubleClickTime = 0)
      val start = new TerminalPosition(0, 0)
      val w = 20
      val h = 8

      // ╒══════════════════1
      // │       ABC        │
      // ╞══════════════════╡
      // │ 2                │
      // │                  │
      // │         3        │
      // │                  │
      // └──────────────────┘

      keyConverter.convertMouseAction(mouseClick(19, 0), start, w, h) shouldBe empty
      keyConverter.convertMouseAction(mouseClick(2, 3), start, w, h) should contain(MouseClick(0, 0, 1))
      keyConverter.convertMouseAction(mouseClick(11, 5), start, w, h) should contain(MouseClick(9, 2, 1))
    }

    test("size 25x10") {
      val converter = KeyConverter(doubleClickTime = 0)
      val start = new TerminalPosition(0, 0)
      val w = 28
      val h = 10

      // 1═══════════════════════╕
      // │          ABC          │
      // ╞═2═════════════════════╡
      // │                     3 │
      // │                       │
      // │                       │
      // │                       │
      // │                4      │
      // │                     5 │
      // └─────6─────────────────┘

      converter.convertMouseAction(mouseClick(0, 0), start, w, h) shouldBe empty
      converter.convertMouseAction(mouseClick(3, 2), start, w, h) shouldBe empty
      converter.convertMouseAction(mouseClick(23, 3), start, w, h) should contain(MouseClick(21, 0, 1))
      converter.convertMouseAction(mouseClick(18, 7), start, w, h) should contain(MouseClick(16, 4, 1))
      converter.convertMouseAction(mouseClick(23, 8), start, w, h) should contain(MouseClick(21, 5, 1))
      converter.convertMouseAction(mouseClick(7, 9), start, w, h) shouldBe empty

    }

    test("with other start position") {
      val keyConverter = KeyConverter(doubleClickTime = 0)
      val start = new TerminalPosition(0, 6)
      val w = 20
      val h = 8

      // ╒══════════════════╕
      // │       ABC        │
      // ╞══════════════════╡
      // │          1       │
      // │               2  │
      // └──────────────────┘
      // ╒══════3═══════════╕
      // │       ABC        4
      // ╞══════════════════╡
      // │ 5                │
      // │       6          │
      // │                  │
      // │        7         │
      // └──────────────────┘

      keyConverter.convertMouseAction(mouseClick(11, 3), start, w, h) shouldBe empty
      keyConverter.convertMouseAction(mouseClick(16, 4), start, w, h) shouldBe empty
      keyConverter.convertMouseAction(mouseClick(7, 6), start, w, h) shouldBe empty
      keyConverter.convertMouseAction(mouseClick(19, 7), start, w, h) shouldBe empty
      keyConverter.convertMouseAction(mouseClick(2, 9), start, w, h) should contain(MouseClick(0, 0, 1))
      keyConverter.convertMouseAction(mouseClick(8, 10), start, w, h) should contain(MouseClick(6, 1, 1))
      keyConverter.convertMouseAction(mouseClick(9, 12), start, w, h) should contain(MouseClick(7, 3, 1))
    }

  }


  it should "create correct mouse double and triple click events" in {
    val doubleClickTime = 50
    val keyConverter = KeyConverter(doubleClickTime)
    val start = new TerminalPosition(0, 0)
    val w = 20
    val h = 8
    // ╒══════════════════╕
    // │       ABC        │
    // ╞══════════════════╡
    // │ 1                │
    // │                  │
    // │                  │
    // │                  │
    // └──────────────────┘

    val click = mouseClick(2, 3)

    keyConverter.convertMouseAction(click, start, w, h) should contain(MouseClick(0, 0, 1))
    keyConverter.convertMouseAction(click, start, w, h) should contain(MouseClick(0, 0, 2))

    Thread.sleep(doubleClickTime)

    keyConverter.convertMouseAction(click, start, w, h) should contain(MouseClick(0, 0, 1))
    keyConverter.convertMouseAction(click, start, w, h) should contain(MouseClick(0, 0, 2))
    keyConverter.convertMouseAction(click, start, w, h) should contain(MouseClick(0, 0, 3))

    Thread.sleep(doubleClickTime)

    // Clicking somewhere else should interrupt the incrementation
    keyConverter.convertMouseAction(click, start, w, h) should contain(MouseClick(0, 0, 1))
    keyConverter.convertMouseAction(click, start, w, h) should contain(MouseClick(0, 0, 2))
    keyConverter.convertMouseAction(mouseClick(2, 4), start, w, h) should contain(MouseClick(0, 1, 1))
    keyConverter.convertMouseAction(click, start, w, h) should contain(MouseClick(0, 0, 1))
  }


  it should "create correct drag events" in {
    val keyConverter = KeyConverter(doubleClickTime = 0)
    val start = new TerminalPosition(0, 0)
    val w = 25
    val h = 10
    // ╒═══════════════════════╕
    // │          ABC          │
    // ╞═══════════════════════╡
    // │                     1 │
    // │                       │
    // │                       │
    // │                       │
    // │                2      │
    // │                     3 │
    // └───────────────────────┘

    keyConverter.convertMouseAction(mouseDrag(0, 0), start, w, h) should contain(MouseDrag(0, 0))
    keyConverter.convertMouseAction(mouseDrag(23, 3), start, w, h) should contain(MouseDrag(21, 0))
    keyConverter.convertMouseAction(mouseDrag(3, 2), start, w, h) should contain(MouseDrag(1, 0))
    keyConverter.convertMouseAction(mouseDrag(7, 9), start, w, h) should contain(MouseDrag(5, 5))
    keyConverter.convertMouseAction(mouseDrag(18, 7), start, w, h) should contain(MouseDrag(16, 4))
    keyConverter.convertMouseAction(mouseDrag(23, 8), start, w, h) should contain(MouseDrag(21, 5))
    keyConverter.convertMouseAction(mouseDrag(26, 6), start, w, h) should contain(MouseDrag(21, 3))
    keyConverter.convertMouseAction(mouseDrag(100, 100), start, w, h) should contain(MouseDrag(21, 5))
  }

  it should "convert key events" in {
    val keyConverter = KeyConverter(0)

    keyConverter.convertKey(new KeyStroke('a', true, false, true)) should contain(
      CharacterKey('a', Ctrl(true), Alt(false), Shift(true)))

    keyConverter.convertKey(new KeyStroke('-', false, true, true)) should contain(
      CharacterKey('-', Ctrl(false), Alt(true), Shift(true)))

    keyConverter.convertKey(new KeyStroke(KeyType.ArrowLeft, true, false, true)) should contain(
      ArrowKey(Direction.Left, Ctrl(true), Alt(false), Shift(true)))

    keyConverter.convertKey(new KeyStroke(KeyType.ArrowRight, false, false, false)) should contain(
      ArrowKey(Direction.Right, Ctrl(false), Alt(false), Shift(false)))

    keyConverter.convertKey(new KeyStroke(KeyType.ArrowUp, true, true, true)) should contain(
      ArrowKey(Direction.Up, Ctrl(true), Alt(true), Shift(true)))

    keyConverter.convertKey(new KeyStroke(KeyType.ArrowDown, true, false, true)) should contain(
      ArrowKey(Direction.Down, Ctrl(true), Alt(false), Shift(true)))

    keyConverter.convertKey(new KeyStroke(KeyType.Enter, false, true, true)) should contain(
      CharacterKey('\n', Ctrl(false), Alt(true), Shift(true)))

    keyConverter.convertKey(new KeyStroke(KeyType.Tab, false, true, true)) should contain(
      CharacterKey('\t', Ctrl(false), Alt(true), Shift(true)))

    keyConverter.convertKey(new KeyStroke(KeyType.Escape, false, true, true)) should contain(
      OtherKey(KeyType.Escape, Ctrl(false), Alt(true), Shift(true)))
  }

  private def mouseClick(x: Int, y: Int) = new MouseAction(MouseActionType.CLICK_DOWN, 1, new TerminalPosition(x, y))
  private def mouseDrag(x: Int, y: Int) = new MouseAction(MouseActionType.DRAG, 1, new TerminalPosition(x, y))


}
