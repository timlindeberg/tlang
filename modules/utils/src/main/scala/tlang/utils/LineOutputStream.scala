package tlang
package utils

import java.io.{OutputStream, PrintStream}
import java.nio.charset.StandardCharsets

object LineOutputStream {
  var OriginalOut      : Option[PrintStream]               = None
  val ThreadLocalStream: ThreadLocal[Option[OutputStream]] = ThreadLocal.withInitial(() => None)

  def redirect(printLine: (String, Int) => Unit): Unit = {
    if (OriginalOut.isEmpty)
      OriginalOut = Some(System.out)

    System.setOut(new LineOutputStream(System.out, printLine))
  }

  def restoreStdout(): Unit = {
    OriginalOut ifDefined System.setOut
    OriginalOut = None
  }
}

class LineOutputStream(originalOut: OutputStream, printLine: (String, Int) => Unit) extends PrintStream(originalOut) {

  private var lineNumber = 1
  private val sb         = new StringBuilder()

  override def write(b: Array[Byte]): Unit = writeString(new String(b, StandardCharsets.UTF_8))
  override def write(buf: Array[Byte], off: Int, len: Int): Unit = writeString(new String(buf, off, len, StandardCharsets.UTF_8))

  override def write(b: Int): Unit = {
    if (b == '\n') {
      writeLine()
    } else {
      sb.append(b.asInstanceOf[Char])
    }
  }

  override def flush(): Unit = {}

  private def writeString(s: String): Unit = {
    var i = 0
    while (i < s.length) {
      if (s(i) == '\n') {
        writeLine()
      } else if (i < s.length - 1 && s(i) == '\r' && s(i + 1) == '\n') {
        writeLine()
        i += 1
      } else {
        sb.append(s(i))
      }
      i += 1
    }
  }

  private def writeLine(): Unit = {
    printLine(sb.toString, lineNumber)
    sb.clear()
    lineNumber += 1
  }
}
