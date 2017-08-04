package tlang.utils

import java.io._
import java.lang.reflect.{InvocationTargetException, Method}
import java.net.{URL, URLClassLoader}

import tlang.Context

import scala.concurrent.duration.Duration

case class ProgramExecutor(timeout: Duration = Duration(0, "sec")) {

  def apply(ctx: Context, classFile: File): String = apply(ctx.classPath.paths, classFile)

  def apply(classPaths: Set[String], classFile: File): String = {
    val mainName = classFile.getName.replaceAll("\\..*", "")
    apply(classPaths, mainName)
  }

  def apply(classPaths: Set[String], programName: String): String = {
    val method = getMainMethod(classPaths, programName)
    executeMethod(method, programName)
  }

  private def executeMethod(method: Method, programName: String): String = {
    // In order to run tests in parallel we use a custom PrintStream
    // to redirect threads started from here to byte output streams.
    // Threads that use println that are not started from here
    // will be redirected to the original Sysout (at the time of redirection).
    // It uses the a thread local byte stream to redirect output to which enables
    // multiple threads to use different output streams.
    // At the end of the block the threads output is redirected back to the original
    // system out.
    try {
      CapturedOutput { method.invoke(null, Array[String]()) }
    } catch {
      case e: InvocationTargetException => throw e.getCause
    }
  }


  private def getMainMethod(classPaths: Set[String], programName: String): Method = {
    val urls = classPaths.map(cp => new URL(s"file:$cp/")).toArray

    val classLoader = new URLClassLoader(urls)

    val clazz = classLoader.loadClass(programName)
    clazz.getMethod("main", classOf[Array[String]])
  }
}

