package koolc

import java.io.File
import koolc.lexer.Token
import scala.sys.process.ProcessLogger

object TestUtils {
  val runScript = "./reference/run.sh"
  val resources = "./src/test/resources/"
  def programFiles(dir: String) = new File(dir).listFiles
  def format(token: Token): String = token + "(" + token.line + ":" + token.col + ")"

  object IgnoreErrorOutput extends ProcessLogger {
    def buffer[T](f: ⇒ T): T = f
    def err(s: ⇒ String): Unit = {}
    def out(s: ⇒ String): Unit = {}
  }

}

