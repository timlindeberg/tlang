package tlang.utils

import java.io.{ByteArrayOutputStream, OutputStream, PrintStream}

import tlang.utils.Extensions._

object CapturedOutput {

  private var originalOut          : Option[PrintStream]               = None
  private val threadLocalByteStream: ThreadLocal[Option[OutputStream]] = ThreadLocal.withInitial(() => None)

  def apply[U](block: => U): String = {
    redirect()
    val byteOutput = new ByteArrayOutputStream()
    threadLocalByteStream.set(Some(byteOutput))
    try block
    finally threadLocalByteStream.set(None)

    byteOutput.toString
  }

  def restoreStdout(): Unit = originalOut ifDefined System.setOut

  private def redirect(): Unit = {
    if (!System.out.isInstanceOf[CapturingOutputStream]) {
      originalOut = Some(System.out)
      System.setOut(new CapturingOutputStream(System.out))
    }
  }


  private class CapturingOutputStream(originalOut: OutputStream) extends PrintStream(originalOut) {

    override def write(b: Array[Byte]): Unit = stream.write(b)
    override def write(b: Int): Unit = stream.write(b)
    override def write(buf: Array[Byte], off: Int, len: Int): Unit = stream.write(buf, off, len)
    override def flush(): Unit = stream.flush()

    private def stream: OutputStream = threadLocalByteStream.get.getOrElse(originalOut)
  }

}
