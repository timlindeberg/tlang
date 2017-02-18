package tlang.compiler.utils

import java.io.File

import tlang.compiler.Context
import tlang.compiler.main.Main

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Future, _}
import scala.sys.process._


/**
  * Created by Tim Lindeberg on 2/3/2017.
  */
case class ProgramExecutor(timeout: Duration = duration.Duration(2, "sec")) {

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
    val cp = formatClassPath(classPaths)
    val command = s"java -cp $cp $mainName"
    val future = Future(blocking(command !!))
    try {
      Some(Await.result(future, timeout))
    } catch {
      case _: TimeoutException => None
    }
  }

  def mainName(file: File): String = file.getName.replaceAll("\\" + Main.FileEnding, "")

  private def formatClassPath(classPaths: List[String]): String = {
    val paths = classPaths.mkString(File.pathSeparator)
    if (System.getProperty("os.name").startsWith("Windows"))
      "\"" + paths + "\""
    else
      paths
  }


}
