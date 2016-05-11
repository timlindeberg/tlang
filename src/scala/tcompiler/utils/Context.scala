package tcompiler
package utils

import java.io.File

case class Context(
  reporter: Reporter,
  file: File,
  classPaths: List[String] = List(),
  outDir: Option[File] = None
  )
