package koolc

import java.io.File
import koolc.lexer.Token

object TestUtils {
  def programFiles(dir: String) = new File(dir).listFiles.filter(_.toString.endsWith(".kool"))
  def format(token: Token): String = token + "(" + token.line + ":" + token.col + ")"
}