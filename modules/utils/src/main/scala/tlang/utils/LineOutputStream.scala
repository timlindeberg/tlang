package tlang
package utils

import java.io.{OutputStream, PrintStream}
import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets

object LineOutputStream {

  var OriginalOut: Option[PrintStream] = None
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
  private val bytes: ByteBuffer = ByteBuffer.allocate(16384)

  override def write(b: Array[Byte]): Unit = write(b, 0, b.length)
  override def write(buf: Array[Byte], off: Int, len: Int): Unit = {
    var index = 0
    while (index < len) {
      val i = off + index
      if (buf(i) == '\n') {
        writeLine()
      } else if (i < buf.length - 1 && buf(i) == '\r' && buf(i + 1) == '\n') {
        writeLine()
        index += 1
      } else {
        bytes.put(buf(i))
      }
      index += 1
    }
  }

  override def write(b: Int): Unit = {
    if (b == '\n') {
      writeLine()
    } else {
      bytes.put(b.asInstanceOf[Byte])
    }
  }

  override def flush(): Unit = {}

  private def writeLine(): Unit = {
    val s = new String(bytes.array(), 0, bytes.position(), StandardCharsets.UTF_8)
    printLine(s, lineNumber)
    bytes.clear()
    lineNumber += 1
  }
}
