package tlang.utils

import java.lang.reflect.{InvocationTargetException, Method}
import java.net.{URL, URLClassLoader}

import better.files.File

case class ExecutionResult(output: String, exception: Option[Throwable] = None)

case class ProgramExecutor(classPaths: Set[String]) {

  private val URLs = classPaths.map(classPath => new URL(s"file:$classPath/")).toArray

  def apply(classFile: File): ExecutionResult = apply(classFile.name.replaceAll("\\..*", ""))
  def apply(className: String): ExecutionResult = execute(className)
  def apply(source: Source): ExecutionResult = execute(source.mainName)

  private def execute(className: String): ExecutionResult = {
    val method = try {
      getMainMethod(className)
    } catch {
      case e: VerifyError => return ExecutionResult("", Some(e))
    }

    // In order to run tests in parallel we use a custom PrintStream to redirect threads started
    // from here to byte output streams. Threads that use println that are not started from here
    // will be redirected to the original Sysout (at the time of redirection). It uses the a thread
    // local byte stream to redirect output to which enables multiple threads to use different output
    // streams. At the end of the block the threads output is redirected back to the original system out.
    var exception: Option[Throwable] = None

    val output = CapturedOutput {
      try method.invoke(null, Array[String]())
      catch {
        case e: InvocationTargetException => exception = Some(e.getCause)
      }
    }
    ExecutionResult(output, exception)
  }


  private def getMainMethod(className: String): Method = {
    val classLoader = new URLClassLoader(URLs)

    val clazz = classLoader.loadClass(className)
    clazz.getMethod("main", classOf[Array[String]])
  }
}

