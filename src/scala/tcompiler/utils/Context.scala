package tcompiler
package utils

import java.io.File

case class Context(
  reporter: Reporter,
  files: List[File],
  classPaths: List[String] = List(),
  outDir: Option[File] = None
  ) {

  def getClassPaths = classPaths :: List(Main.TDirectory, ".")
}
