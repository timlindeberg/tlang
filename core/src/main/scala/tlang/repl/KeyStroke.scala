package tlang.repl

import com.googlecode.lanterna.input.KeyType


case class KeyStroke(keyType: KeyType, char: Character, isCtrlDown: Boolean, isAltDown: Boolean, isShiftDown: Boolean)
