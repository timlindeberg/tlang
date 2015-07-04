package tcompiler
package utils

import java.io.File

case class Context(
  val reporter: Reporter,
  val outDir: Option[File],
  val file: File)
