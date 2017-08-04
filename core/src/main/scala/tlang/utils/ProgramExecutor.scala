package tlang.utils

import java.io._
import java.lang.reflect.{InvocationTargetException, Method}
import java.net.{URL, URLClassLoader}

import tlang.Context
import tlang.utils.Extensions._

import scala.concurrent.duration.Duration


case class ProgramExecutor(timeout: Duration = Duration(0, "sec")) {

  def apply(ctx: Context, classFile: File): String = apply(ctx.classPath.paths, classFile)

  def apply(classPaths: Set[String], classFile: File): String = {
    val mainName = classFile.getName.replaceAll("\\..*", "")
    apply(classPaths, mainName)
  }

  def apply(classPaths: Set[String], mainName: String): String = {
    val method = getMainMethod(classPaths, mainName)
    stdoutOutput {
      withTimeout(timeout) {
        try {
          method.invoke(null, Array[String]())
        } catch {
          case e: InvocationTargetException => throw e.getCause
        }
      }
    }
  }

  private def getMainMethod(classPaths: Set[String], mainName: String): Method = {
    val urls = classPaths.map(cp => new URL(s"file:$cp/")).toArray

    val classLoader = new URLClassLoader(urls)

    val clazz = classLoader.loadClass(mainName)
    clazz.getMethod("main", classOf[Array[String]])
  }
}
