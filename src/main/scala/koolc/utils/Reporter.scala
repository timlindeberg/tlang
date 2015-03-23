package koolc
package utils

import java.io.File
import scala.io.Source

class ParsingException(msg: String) extends RuntimeException(msg)

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
    throw new ParsingException(errMessage("Fatal", msg, pos))
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

  private def report(prefix: String, msg: Any, pos: Positioned) = System.err.println(errMessage(prefix, msg, pos))
  
  private def errMessage(prefix: String, msg: Any, pos: Positioned): String = {
    var s = ""
    if (pos.hasPosition) {
      s += s + pos.position + ": " + prefix + ": " + msg.toString + "\n"

      val lines = getLines(pos.file)

      if (pos.line-1 < lines.size) {
          s += lines(pos.line-1) + "\n"
          s += " "*(pos.col-1)+"^" + "\n"
      } else {
          s += "<line unavailable in source file>"
      }
    } else {
      s += prefix+": "+msg.toString
    }
    s
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
