package tlang
package utils

import java.io.{ByteArrayOutputStream, OutputStream, PrintStream}

object CapturingOutputStream {
  private var OriginalOut: Option[PrintStream] = None
  private val ThreadLocalStream: ThreadLocal[Option[OutputStream]] = ThreadLocal.withInitial(() => None)

  def byteStream: ByteArrayOutputStream = {
    val byteOutput = new ByteArrayOutputStream()
    CapturingOutputStream.ThreadLocalStream.set(Some(byteOutput))
    byteOutput
  }

  def clearByteStream(): Unit = {
    CapturingOutputStream.ThreadLocalStream.set(None)
  }

  def redirect(): Unit = {
    if (!System.out.isInstanceOf[CapturingOutputStream]) {
      CapturingOutputStream.OriginalOut = Some(System.out)
      System.setOut(new CapturingOutputStream(System.out))
    }
  }

  def restoreStdout(): Unit = OriginalOut ifDefined System.setOut
}

class CapturingOutputStream(originalOut: OutputStream) extends PrintStream(originalOut) {

  override def write(b: Array[Byte]): Unit = stream.write(b)
  override def write(b: Int): Unit = stream.write(b)
  override def write(buf: Array[Byte], off: Int, len: Int): Unit = stream.write(buf, off, len)
  override def flush(): Unit = stream.flush()

  private def stream: OutputStream = CapturingOutputStream.ThreadLocalStream.get.getOrElse(originalOut)
}
