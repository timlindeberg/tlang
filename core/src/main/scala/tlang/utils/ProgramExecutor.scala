package tlang.utils

import java.io._
import java.lang.reflect.Method
import java.net.{URL, URLClassLoader}

import tlang.compiler.Context
import tlang.utils.Extensions._

import scala.concurrent.duration.Duration

case class ProgramExecutor(timeout: Option[Duration] = None) {

  def apply(ctx: Context, classFile: File): String = {
    apply(ctx.getClassPaths.toList, classFile)
  }
  
  def apply(classPaths: List[String], classFile: File): String = {
    val mainName = classFile.getName.replaceAll("\\..*", "")
    apply(classPaths, mainName)
  }

  def apply(classPaths: List[String], mainName: String): String = {
    val method = getMainMethod(classPaths, mainName)
    stdoutOutput {
      withTimeout(timeout.getOrElse(Duration(0, "sec"))) {
        method.invoke(null, Array[String]())
      }
    }
  }

  private def getMainMethod(classPaths: List[String], mainName: String): Method = {
    val urls = classPaths.map(cp => new URL(s"file:$cp/")).toArray

    val classLoader = new URLClassLoader(urls)

    val clazz = classLoader.loadClass(mainName)
    clazz.getMethod("main", classOf[Array[String]])
  }
}
