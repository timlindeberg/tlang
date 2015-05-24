package koolc
package utils

import java.io.File
import scala.io.Source

class CompilationException(msg: String) extends RuntimeException(msg)

class Reporter(quiet: Boolean = false, ignoreFirstLine: Boolean = false) {

  def info(msg: Any, pos: Positioned = NoPosition): Unit = {
    report("Info", msg, pos)
  }

  def warning(msg: Any, pos: Positioned = NoPosition): Unit = {
    report("Warning", msg, pos)
  }

  var errors = 0
  def hasErrors = errors > 0

  def error(msg: Any, pos: Positioned = NoPosition): Unit = {
    errors += 1
    report("Error", msg, pos)
  }

  def fatal(msg: Any, pos: Positioned = NoPosition): Nothing = {
    throw new CompilationException(errMessage("Fatal", msg, pos))
  }

  var filesToLines = Map[File, IndexedSeq[String]]()

  private def err(msg: String) {
    System.err.println(msg)
  }

  def terminateIfErrors = {
    if (hasErrors && !quiet) {
      err("There were errors.")
      sys.exit(1)
    }
  }
  
  private def templateName(name: String): String = {
    val s = name.split('$')
    if(s.length <= 1) name
    else s.tail.mkString(s.head + "[", ", ", "]")
  }

  private def replaceTemplateNames(msg: String) = msg.split(' ').map(templateName).mkString(" ")
  
  private def report(prefix: String, msg: Any, pos: Positioned) = { if (!quiet) System.err.println(errMessage(prefix, msg, pos)) }

  private def errMessage(prefix: String, msg: Any, pos: Positioned): String = {
    var s = ""
    
    var msgStr = replaceTemplateNames(msg.toString)
    if (pos.hasPosition) {
      s += s + pos.position + ": " + prefix + ": " + msgStr + "\n"

      val lines = getLines(pos.file)

      if (pos.line - 1 < lines.size) {
        s += lines(pos.line - 1) + "\n"
        s += " " * (pos.col - 1) + "^" + "\n"
      } else {
        s += "<line unavailable in source file>"
      }
    } else {
      s += prefix + ": " + msgStr
    }
    s
  }

  private def getLines(f: File): IndexedSeq[String] = {
    filesToLines.get(f) match {
      case Some(lines) =>
        lines

      case None =>
        val source = Source.fromFile(f).withPositioning(true)
        var lines = source.getLines().toIndexedSeq
        if (ignoreFirstLine) {
          lines = lines.tail
        }
        source.close()

        filesToLines += f -> lines

        lines
    }
  }
}
