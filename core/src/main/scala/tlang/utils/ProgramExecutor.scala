package tlang.utils

import java.io._
import java.lang.reflect.{InvocationTargetException, Method}
import java.net.{URL, URLClassLoader}

import tlang.Context

import scala.concurrent.duration.Duration

object ProgramExecutor {
  def apply(context: Context, timeout: Duration = Duration(0, "sec")): ProgramExecutor = {
    new ProgramExecutor(context.outDirs.map(_.getAbsolutePath) ++ context.classPath.paths, timeout)
  }
}

case class ProgramExecutor(classPaths: Set[String], timeout: Duration) {

  private val URLs = classPaths.map(classPath => new URL(s"file:$classPath/")).toArray

  def apply(classFile: File): String = apply(classFile.getName.replaceAll("\\..*", ""))

  def apply(className: String): String = execute(className)

  private def execute(className: String): String = {
    val method = getMainMethod(className)

    // In order to run tests in parallel we use a custom PrintStream to redirect threads started
    // from here to byte output streams. Threads that use println that are not started from here
    // will be redirected to the original Sysout (at the time of redirection). It uses the a thread
    // local byte stream to redirect output to which enables multiple threads to use different output
    // streams. At the end of the block the threads output is redirected back to the original system out.
    try {
      CapturedOutput { method.invoke(null, Array[String]()) }
    } catch {
      case e: InvocationTargetException =>
        // We're rethrowing the cause since the wrapping InvocationTargetException is not very interesting
        throw e.getCause
    }
  }


  private def getMainMethod(className: String): Method = {
    val classLoader = new URLClassLoader(URLs)

    val clazz = classLoader.loadClass(className)
    clazz.getMethod("main", classOf[Array[String]])
  }
}

