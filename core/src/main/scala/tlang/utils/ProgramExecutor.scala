package tlang.utils

import java.io._
import java.lang.reflect.Method
import java.net.{URL, URLClassLoader}

import tlang.compiler.{Context, Main}
import tlang.utils.Extensions._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Future, _}


/**
  * Created by Tim Lindeberg on 2/3/2017.
  */
case class ProgramExecutor(timeout: Duration = duration.Duration(2, "sec")) {

  def apply(classFilePath: String): Option[String] = apply(new File(classFilePath))

  def apply(classFile: File): Option[String] = {
    val path = classFile.getParent
    val mainName = classFile.getName.replaceAll("\\.class", "")
    apply(List(path, Main.TDirectory), mainName)
  }

  def apply(ctx: Context, testFile: File): Option[String] = {
    val name = mainName(testFile)
    val path = ctx.outDirs.head.getAbsolutePath
    apply(List(path, Main.TDirectory), name)
  }

  def apply(classPaths: List[String], mainName: String): Option[String] = {
    val meth = getMainMethod(classPaths, mainName)
    val future = Future(blocking(executeMainMethod(meth)))

    try {
      Some(Await.result(future, timeout))
    } catch {
      case _: TimeoutException => None
      case e: Exception        =>
        println(s"Got an exception while executing $mainName:")
        e.printStackTrace()
        None
    }
  }

  private def getMainMethod(classPaths: List[String], mainName: String): Method = {
    val urls = classPaths.map(cp => new URL(s"file:$cp/")).toArray
    val classLoader = new URLClassLoader(urls)

    val clazz = classLoader.loadClass(mainName)
    clazz.getMethod("main", classOf[Array[String]])
  }

  private def executeMainMethod(method: Method): String =
    stdoutOutput {
      method.invoke(null, Array[String]())
    }

  def mainName(file: File): String = file.getName.replaceAll("\\" + Main.FileEnding, "")

}
