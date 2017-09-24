package tlang.repl.terminal

import java.util

import com.googlecode.lanterna.input.CharacterPattern.Matching
import com.googlecode.lanterna.input.{CharacterPattern, KeyDecodingProfile, KeyStroke, KeyType}
import tlang.utils.Extensions._


object CustomCharacterPatterns extends KeyDecodingProfile {

  override val getPatterns: util.List[CharacterPattern] = util.Arrays.asList(BackwardsForwardsByWord, Redo)

  // Translates '[f' to Alt-Right and '[b' to Alt-Left and '^H' to Alt-Backspace
  object BackwardsForwardsByWord extends CharacterPattern {
    override def `match`(seq: util.List[Character]): Matching = {
      val size = seq.size

      if (seq.get(0) != KeyDecodingProfile.ESC_CODE)
        return null

      size match {
        case 1 => Matching.NOT_YET
        case 2 =>
          seq.get(1).charValue() match {
            case 102 => new Matching(new KeyStroke(KeyType.ArrowRight, false, true, false))
            case 98  => new Matching(new KeyStroke(KeyType.ArrowLeft, false, true, false))
            case 8   => new Matching(new KeyStroke(KeyType.Backspace, false, true, false))
            case _   => null
          }
        case _ =>
          null
      }
    }
  }

  object Redo extends CharacterPattern {
    override def `match`(seq: util.List[Character]): Matching = {
      val size = seq.size

      if (seq.get(0) != 18)
        return null

      if(size < 2)
        return Matching.NOT_YET

      if(seq.get(1) != 31)
        return null

      new Matching(new KeyStroke('_', true, false, true))
    }
  }
}


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

case class MouseDown(override val x: Int, override val y: Int) extends MouseEvent
case class MouseUp(override val x: Int, override val y: Int) extends MouseEvent
case class MouseDrag(override val x: Int, override val y: Int) extends MouseEvent
