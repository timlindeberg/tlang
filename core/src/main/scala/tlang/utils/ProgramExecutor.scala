package tlang.utils

import java.io._
import java.lang.reflect.Method
import java.net.{URL, URLClassLoader}

import tlang.compiler.{Context, Main}
import tlang.utils.Extensions._

import scala.concurrent._
import scala.concurrent.duration.Duration

/**
  * Created by Tim Lindeberg on 2/3/2017.
  */
case class ProgramExecutor(timeout: Option[Duration] = Some(duration.Duration(2, "sec"))) {

  def apply(classFilePath: String): String = apply(new File(classFilePath))

  def apply(classFile: File): String = {
    val path = classFile.getParent
    val mainName = classFile.getName.replaceAll("\\.class", "")
    apply(List(path, Main.TDirectory), mainName)
  }

  def apply(ctx: Context, testFile: File): String = {
    val name = mainName(testFile)
    val path = ctx.outDirs.head.getAbsolutePath
    apply(List(path, Main.TDirectory), name)
  }

  def apply(classPaths: List[String], mainName: String): String = {
    val method = getMainMethod(classPaths, mainName)

    stdoutOutput {
      timeout match {
        case Some(t) => withTimeout(t) {method.invoke(null, Array[String]())}
        case None    => method.invoke(null, Array[String]())
      }
    }
  }

  private def getMainMethod(classPaths: List[String], mainName: String): Method = {
    val urls = classPaths.map(cp => new URL(s"file:$cp/")).toArray
    val classLoader = new URLClassLoader(urls)

    val clazz = classLoader.loadClass(mainName)
    clazz.getMethod("main", classOf[Array[String]])
  }

  private def mainName(file: File): String = file.getName.replaceAll("\\" + Main.FileEnding, "")

}
