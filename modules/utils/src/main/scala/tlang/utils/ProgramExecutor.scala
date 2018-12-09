package tlang
package utils

import java.lang.reflect.{InvocationTargetException, Method}
import java.net.{URL, URLClassLoader}

import better.files.File

case class ExecutionResult(output: String, time: Double, exception: Option[Throwable] = None)

trait ProgramExecutor {
  def classPaths: Set[String]
  def apply(classFile: File): ExecutionResult = apply(classFile.name.replaceAll("\\..*", ""))
  def apply(className: String): ExecutionResult = _execute(className)
  def apply(source: Source): ExecutionResult = _execute(source.mainName)

  protected def execute(method: Method, className: String): ExecutionResult

  private def _execute(className: String): ExecutionResult = {
    val method = try {
      getMainMethod(className)
    } catch {
      case e: VerifyError => return ExecutionResult("", 0, Some(e))
    }
    execute(method, className)
  }

  private def getMainMethod(className: String): Method = {
    val URLs = classPaths.map(classPath => new URL(s"file:$classPath/")).toArray

    val classLoader = new URLClassLoader(URLs)

    val clazz = classLoader.loadClass(className)
    clazz.getMethod("main", classOf[Array[String]])
  }
}

case class DefaultProgramExecutor(classPaths: Set[String]) extends ProgramExecutor {

  protected def execute(method: Method, className: String): ExecutionResult = {
    // In order to run tests in parallel we use a custom PrintStream to redirect threads started
    // from here to byte output streams. Threads that use println that are not started from here
    // will be redirected to the original Sysout (at the time of redirection). It uses the a thread
    // local byte stream to redirect output to which enables multiple threads to use different output
    // streams. At the end of the block the threads output is redirected back to the original system out.
    var exception: Option[Throwable] = None
    val (output, time) = measureTime {
      capturedOutput {
        try method.invoke(null, Array[String]())
        catch {
          case e: InvocationTargetException => exception = Some(e.getCause)
        }
      }
    }
    ExecutionResult(output, time, exception)
  }

  private def capturedOutput[U](block: => U): String = {
    CapturingOutputStream.redirect()
    val byteOutput = CapturingOutputStream.byteStream
    try block
    finally CapturingOutputStream.clearByteStream()

    byteOutput.toString
  }
}


case class StreamingProgramExecutor(classPaths: Set[String], lineHandler: (String, Int) => Unit) extends ProgramExecutor {

  protected def execute(method: Method, className: String): ExecutionResult = {
    var exception: Option[Throwable] = None

    val (_, time) = measureTime {
      streamingOutput {
        try method.invoke(null, Array[String]())
        catch {
          case e: InvocationTargetException => exception = Some(e.getCause)
        }
      }
    }
    ExecutionResult("", time, exception)
  }

  private def streamingOutput[U](block: => U): Unit = {
    LineOutputStream.redirect(lineHandler)
    try block
    finally CapturingOutputStream.restoreStdout()
  }
}

