package koolc
package utils

import java.io.File
import scala.io.Source

class CompilationException() extends Exception

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
    report("fatal", msg, pos)
    throw new CompilationException
  }

  var filesToLines = Map[File, IndexedSeq[String]]()

  private def err(msg: String) {
    System.err.println(msg)
  }

  def terminateIfErrors = {
    if (hasErrors) {
      if (!quiet) err("There were errors.")
      throw new CompilationException
    }
  }

  private def templateName(name: String): String = {
    val start = name.indexOf("-")
    val end = name.lastIndexOf("-")
    val size = name.length

    if (start != -1 && end != -1) {
      val pre = if (start > 0) name.substring(0, start) else ""
      val mid = name.substring(start + 1, end);
      val post = if (end < size - 1) name.substring(end + 1, size - 1) else ""

      templateName(pre + templateName(mid) + post)
    } else if (name.contains('$')) {
      val s = name.split('$')
      s.tail.mkString(s.head + "<", ", ", ">")
    } else {
      name
    }
  }

  private def replaceTemplateNames(msg: String) = msg.split(' ').map(templateName).mkString(" ")

  private def report(prefix: String, msg: Any, pos: Positioned) = if (!quiet) err(errMessage(prefix, msg, pos))

  private def errMessage(prefix: String, msg: Any, pos: Positioned): String = {
    var s = ""
    val msgStr = msg.toString//replaceTemplateNames(msg.toString)
    if (pos.hasPosition) {
      s += s + pos.position + ": " + prefix + ": " + msgStr + "\n"

      val lines = getLines(pos.file)

      if (pos.line - 1 < lines.size) {
        val line = lines(pos.line - 1)
        val tabs = line.count(_ == '\t')

        s += line.replaceAll("\t", "    ") + "\n"
        s += " " * (pos.col - 1 + (tabs * 3)) + "^" + "\n"
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
