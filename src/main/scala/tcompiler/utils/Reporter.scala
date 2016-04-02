package tcompiler
package utils

import java.io.File
import scala.collection.mutable.ArrayBuffer
import scala.io.Source

class CompilationException(message: String) extends Exception(message)

class Reporter(quiet: Boolean = false, ignoreFirstLine: Boolean = false) {

  def info(msg: Any, pos: Positioned = NoPosition): Unit = {
    report("Info", "", msg, pos)
  }

  def warning(errorPrefix: String, errorCode: Int, msg: Any, pos: Positioned = NoPosition): Unit = {
    report("Warning", constructErrorCode(errorPrefix, 1, errorCode), msg, pos)
  }

  var errors = ArrayBuffer[String]()

  def clearErrors() = errors.clear()
  def hasErrors = errors.nonEmpty

  def error(errorPrefix: String, errorCode: Int, msg: String, pos: Positioned = NoPosition): Unit =
    errors += report("Error", constructErrorCode(errorPrefix, 2, errorCode), msg, pos)


  def fatal(errorPrefix: String, errorCode: Int, msg: String, pos: Positioned = NoPosition): Nothing = {
    val error = report("Fatal", constructErrorCode(errorPrefix, 3, errorCode), msg, pos)
    throw new CompilationException(error)
  }

  var filesToLines = Map[File, IndexedSeq[String]]()

  def terminateIfErrors() =
    if (hasErrors) {
      if (!quiet) System.err.println("There were errors.")
      throw new CompilationException(errors.mkString("\n\n"))
    }

  private def constructErrorCode(prefix: String, num: Int, code: Int) = prefix + num + leftPad(code)

  private def leftPad(num: Int) = num match {
    case x if x >= 0 && x < 10     => "00" + x
    case x if x >= 10 && x < 100   => "0" + x
    case x if x >= 100 && x < 1000 => x
    case _                         => ???
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

  private def report(prefix: String, errorCode: String, msg: Any, pos: Positioned) ={
    val error = errMessage(prefix, errorCode, msg, pos)
    if(!quiet)
      System.err.println(error)
    error
  }


  private def errMessage(prefix: String, errorCode: String, msg: Any, pos: Positioned): String = {
    val msgStr = msg.toString //replaceTemplateNames(msg.toString)
    var s = ""
    if (pos.hasPosition) {
      s += s"[${pos.position}] $prefix ($errorCode): $msgStr \n"
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
