package tlang
package repl
package terminal

import com.googlecode.lanterna.TerminalPosition
import com.googlecode.lanterna.input.{KeyStroke, KeyType, MouseAction, MouseActionType}

case class KeyConverter(doubleClickTime: Long) {

  private var lastMouseClick: (Int, Int, Long) = (0, 0, 0L)
  private var numClicks: Int = 1

  def convertKey(key: KeyStroke): Option[Key] = {
    val ctrl = Ctrl(key.isCtrlDown)
    val alt = Alt(key.isAltDown)
    val shift = Shift(key.isShiftDown)

    val k = key.getKeyType match {
      case KeyType.Character  => CharacterKey(key.getCharacter, ctrl, alt, shift)
      case KeyType.Enter      => CharacterKey('\n', ctrl, alt, shift)
      case KeyType.Tab        => CharacterKey('\t', ctrl, alt, shift)
      case KeyType.ArrowUp    => ArrowKey(Direction.Up, ctrl, alt, shift)
      case KeyType.ArrowDown  => ArrowKey(Direction.Down, ctrl, alt, shift)
      case KeyType.ArrowLeft  => ArrowKey(Direction.Left, ctrl, alt, shift)
      case KeyType.ArrowRight => ArrowKey(Direction.Right, ctrl, alt, shift)
      case _                  => OtherKey(key.getKeyType, ctrl, alt, shift)
    }
    Some(k)
  }

  def convertMouseAction(mouseAction: MouseAction, boxStartPosition: TerminalPosition, boxWidth: Int, boxHeight: Int): Option[Key] = {
    val actionType = mouseAction.getActionType

    // We don't care about the move and release events for now
    if (mouseAction.getActionType in Seq(MouseActionType.MOVE, MouseActionType.CLICK_RELEASE))
      return None

    if (actionType == MouseActionType.SCROLL_DOWN)
      return Some(ArrowKey(Direction.Down, Ctrl(false), Alt(false), Shift(false)))

    if (actionType == MouseActionType.SCROLL_UP)
      return Some(ArrowKey(Direction.Up, Ctrl(false), Alt(false), Shift(false)))

    // Only support left click. CLICK_RELEASE always has button == 0.
    val button = mouseAction.getButton
    if ((actionType in Seq(MouseActionType.CLICK_DOWN, MouseActionType.DRAG)) && button != 1)
      return None

    val startOfBuffer = boxStartPosition
      .withRelativeRow(OutputBox.YIndent)
      .withRelativeColumn(OutputBox.XIndent)

    val width = 1 + boxWidth - OutputBox.XIndent * 2
    val height = boxHeight - (OutputBox.YIndent + 1)

    val mousePos = mouseAction.getPosition
    val x = mousePos.getColumn - startOfBuffer.getColumn
    val y = mousePos.getRow - startOfBuffer.getRow

    mouseAction.getActionType match {
      case MouseActionType.CLICK_DOWN =>
        if ((x notIn (0 until width)) || (y notIn (0 until height)))
          return None

        val time = System.currentTimeMillis

        lastMouseClick match {
          case (`x`, `y`, lastTime) if time - lastTime < doubleClickTime => numClicks += 1
          case _                                                         => numClicks = 1
        }

        lastMouseClick = (x, y, time)
        Some(MouseClick(x, y, numClicks))
      case MouseActionType.DRAG       =>
        Some(MouseDrag(x.clamp(0, width - 1), y.clamp(0, height - 1)))
      case _                          => ???
    }
  }
}
