package koolc
package utils

import java.io.File
import scala.io.Source

class Reporter {

  def info(msg: Any, pos: Positioned = NoPosition): Unit = {
    report("Info", msg, pos)
  }

  def warning(msg: Any, pos: Positioned = NoPosition): Unit = {
    report("Warning", msg, pos)
  }

  var hasErrors = false

  def error(msg: Any, pos: Positioned = NoPosition): Unit = {
    hasErrors = true
    report("Error", msg, pos)
  }

  def fatal(msg: Any, pos: Positioned = NoPosition): Nothing = {
    report("Fatal", msg, pos)
    sys.exit(1);
  }

  var filesToLines = Map[File, IndexedSeq[String]]()

  private def err(msg: String) {
    System.err.println(msg)
  }

  def terminateIfErrors = {
    if (hasErrors) {
      err("There were errors.")
      sys.exit(1);
    }
  }

  private def report(prefix: String, msg: Any, pos: Positioned) {
    if (pos.hasPosition) {
      err(pos.position + ": " + prefix + ": " + msg.toString)

      val lines = getLines(pos.file)

      if (pos.line-1 < lines.size) {
          err(lines(pos.line-1))
          err(" "*(pos.col-1)+"^")
      } else {
          err("<line unavailable in source file>")
      }
    } else {
      err(prefix+": "+msg.toString)
    }
  }

  private def getLines(f: File): IndexedSeq[String] = {
    filesToLines.get(f) match {
      case Some(lines) =>
        lines

      case None =>
        val source = Source.fromFile(f).withPositioning(true)
        val lines = source.getLines().toIndexedSeq
        source.close()

        filesToLines += f -> lines

        lines
    }
  }
}
