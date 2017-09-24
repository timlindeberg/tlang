package tlang.repl.terminal

import com.googlecode.lanterna.input.KeyType


case class Ctrl(isDown: Boolean)
case class Alt(isDown: Boolean)
case class Shift(isDown: Boolean)

trait Key {

  def ctrl: Ctrl
  def alt: Alt
  def shift: Shift

  def isCtrlDown: Boolean = ctrl.isDown
  def isAltDown: Boolean = alt.isDown
  def isShiftDown: Boolean = shift.isDown
}

case class CharacterKey(
  char: Char,
  override val ctrl: Ctrl,
  override val alt: Alt,
  override val shift: Shift) extends Key

trait Direction
object Direction {
  case object Up extends Direction
  case object Down extends Direction
  case object Left extends Direction
  case object Right extends Direction
}

case class ArrowKey(
  direction: Direction,
  override val ctrl: Ctrl,
  override val alt: Alt,
  override val shift: Shift) extends Key

case class OtherKey(
  keyType: KeyType,
  override val ctrl: Ctrl,
  override val alt: Alt,
  override val shift: Shift) extends Key

trait MouseEvent extends Key {
  def x: Int
  def y: Int

  override val ctrl : Ctrl  = Ctrl(false)
  override val alt  : Alt   = Alt(false)
  override val shift: Shift = Shift(false)
}

case class MouseClickDown(override val x: Int, override val y: Int) extends MouseEvent
case class MouseClickUp(override val x: Int, override val y: Int) extends MouseEvent
case class MouseDrag(override val x: Int, override val y: Int) extends MouseEvent
