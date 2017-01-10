package tcompiler
package utils

import java.io.File
import java.util.regex.Matcher

import tcompiler.analyzer.Symbols.Symbolic
import tcompiler.analyzer.Types.Typed
import tcompiler.imports.ImportMap

import scala.collection.mutable
import scala.io.Source
import scala.util.parsing.combinator.RegexParsers

class CompilationException(message: String) extends Exception(message)

class Reporter(suppressWarnings: Boolean = false,
               warningIsError: Boolean = false,
               override val useColor: Boolean = false,
               maxErrors: Int = 100)
  extends Colored {

  override def NumColor = Blue
  def QuoteColor = Magenta
  def MessageStyle = Bold
  def WarningColor = Yellow
  def ErrorColor = Red + Bold
  def FatalColor = Red

  private val ErrorSeparator = "\n"
  private val filesToLines   = mutable.Map[File, IndexedSeq[String]]()

  private var hitMaxErrors = false

  var errors   = mutable.LinkedHashSet[String]()
  var warnings = mutable.LinkedHashSet[String]()


  def warning(errorPrefix: String, errorCode: Int, msg: String, pos: Positioned, importMap: ImportMap): Unit = {
    if (warningIsError) {
      error(errorPrefix, errorCode, msg, pos, importMap)
      return
    }

    if (suppressWarnings)
      return

    val warning = errMessage(errorPrefix, 1, errorCode, msg, pos, importMap)
    if(warnings.contains(warning))
      return

    warnings += warning
  }


  def error(errorPrefix: String, errorCode: Int, msg: String, pos: Positioned, importMap: ImportMap): Unit = {
    if (!isValidError(msg, pos))
      return

    if(maxErrors != -1 && errors.size >= maxErrors){
      hitMaxErrors = true
      return
    }

    val err = errMessage(errorPrefix, 2, errorCode, msg, pos, importMap)
    if(errors.contains(err))
      return

    errors += err
  }


  def fatal(errorPrefix: String, errorCode: Int, msg: String, pos: Positioned, importMap: ImportMap): Nothing = {
    val error = errMessage(errorPrefix, 3, errorCode, msg, pos, importMap)
    errors += error
    throw new CompilationException(error)
  }

  def clear() = {
    errors.clear()
    warnings.clear()
    hitMaxErrors = false
  }

  def hasErrors = errors.nonEmpty
  def hasWarnings = warnings.nonEmpty

  def errorsString: String = {
    val err = errors.mkString(ErrorSeparator)
    val numErrors = errors.size
    val num = s"$ErrorColor$numErrors$Reset"

    val prefix = if (hitMaxErrors)
      s"There were more than $num errors, only showing the first $num"
    else if(numErrors == 1)
      s"There was $num error"
    else
      s"There were $num errors"

    prefix + s":\n\n$err"
  }

  def warningsString = warnings.mkString(ErrorSeparator)

  def terminateIfErrors() =
    if (hasErrors)
      throw new CompilationException(errorsString)


  private def isValidError(msg: String, pos: Positioned): Boolean = {
    if (msg.contains(Errors.ErrorName))
      return false

    pos match {
      case t: Typed if t.getType.name == Errors.ErrorName                        => false
      case s: Symbolic[_] if s.hasSymbol && s.getSymbol.name == Errors.ErrorName => false
      case _                                                                     => true
    }
  }

  private def errMessage(locationPrefix: String, errorLevel: Int, errorCode: Int, msg: Any, pos: Positioned, importMap: ImportMap) = {
    val prefix = constructErrorPrefix(locationPrefix, errorLevel, errorCode)
    val msgStr = handleQuoteLiterals(msg.toString, importMap)
    var sb = new StringBuilder
    if (pos.hasPosition)
      sb ++= filePrefix(pos) + "\n"

    sb ++= s"$prefix: $msgStr"

    if (pos.hasPosition) {
      sb ++= locationIndicator(errorLevel, pos)
    }

    sb.toString()
  }

  private def constructErrorPrefix(errorPrefix: String, errorLevel: Int, errorCode: Int) = {
    val code = leftPad(errorCode)

    errorLevel match {
      case 1 => s"${WarningColor}Warning$Reset ($WarningColor${errorPrefix}1$code$Reset)"
      case 2 => s"${ErrorColor}Error$Reset ($ErrorColor${errorPrefix}2$code$Reset)"
      case 3 => s"${FatalColor}Fatal$Reset ($FatalColor${errorPrefix}3$code$Reset)"
      case _ => ???
    }
  }

  private def leftPad(num: Int) = num match {
    case x if x >= 0 && x < 10     => "00" + x
    case x if x >= 10 && x < 100   => "0" + x
    case x if x >= 100 && x < 1000 => x
    case _                         => ???
  }

  private def filePrefix(pos: Positioned) = {
    val Style = Bold + NumColor
    var position = pos.position
    if (useColor) {
      val fileName = pos.file.getName.replaceAll(Main.FileEnding, "")
      position = position.replaceAll("(\\d)", s"$Style$$1$Reset")
      position = position.replaceAll(fileName, s"$Style$fileName$Reset")
    }

    s"$Bold[$Reset$position$Bold]$Reset"
  }

  private def handleQuoteLiterals(msg: String, importMap: ImportMap) = {
    val msgFormat = Reset + MessageStyle

    val re = """'(.+?)'""".r

    val s = re.replaceAllIn(msg, m => {
      var name = importMap.getErrorName(m.group(1))
      name = TemplateNameParser.parseTemplateName(name)
      Matcher.quoteReplacement("\'" + QuoteColor + name + msgFormat + "\'") // escape dollar signs etc.
    })
    msgFormat + s + Reset
  }

  private def locationIndicator(errorLevel: Int, pos: Positioned): String = {
    val lines = getLines(pos.file)
    val numColor = Bold + NumColor
    val prefix = s"$numColor${pos.line}$Reset:   "

    var sb = new StringBuilder
    sb ++= "\n" ++ prefix


    if (pos.line - 1 >= lines.size)
      return sb.toString() + "<line unavailable in source file>"

    val line = lines(pos.line - 1)
    val firstNonWhiteSpace = line.indexWhere(c => !c.isWhitespace)

    val leftTrimmedLine = line.substring(firstNonWhiteSpace)

    val start = pos.col - firstNonWhiteSpace - 1
    var end = if (pos.endLine == pos.line) {
      pos.endCol - firstNonWhiteSpace - 1
    } else {
      val firstSlash = leftTrimmedLine.indexOf("//")
      if (firstSlash != -1)
        firstSlash - 1
      else
        leftTrimmedLine.length
    }


    // Back cursor so we never underline whitespaces
    while (leftTrimmedLine(end - 1).isWhitespace)
      end -= 1


    if (useColor) {
      val errorStyle = errorLevel match {
        case 1 => WarningColor
        case 2 => ErrorColor
        case 3 => FatalColor
        case _ => ???
      }

      val pre = leftTrimmedLine.substring(0, start)
      val highlighted = leftTrimmedLine.substring(start, end)
      val post = leftTrimmedLine.substring(end, leftTrimmedLine.length)
      sb ++= pre ++ Underline ++ errorStyle ++ highlighted ++ Reset ++ post
    } else {
      sb ++= leftTrimmedLine ++ "\n" ++ " " * (start + prefix.length) ++ "~" * (end - start)
    }
    sb ++= "\n"
    sb.toString
  }


  private def getLines(f: File): IndexedSeq[String] =
    filesToLines.getOrElseUpdate(f, {
      val source = Source.fromFile(f).withPositioning(true)
      val lines = source.getLines().toIndexedSeq
      source.close()
      lines
    })
}

object TemplateNameParser extends RegexParsers {

  import tcompiler.modification.Templates._

  def parseTemplateName(s: String): String = {
    val first = s.indexOf('-')
    if (first == -1)
      return s

    val last = s.lastIndexOf('-')
    if (last == -1)
      return s

    val preFix = s.substring(0, first)
    val middle = s.substring(first, last + 1)
    val postFix = s.substring(last + 1, s.length)

    parseAll(template, middle) match {
      case Success(res, _) => preFix + res + postFix
      case Failure(_, _)   => s
      case _               => ???
    }
  }

  // Grammar

  private def word: Parser[String] =
    """[a-zA-Z\d_]+""".r ^^ {
      _.toString
    }

  private def typeList: Parser[String] = ((Seperator ~ (word | template)) +) ^^ {
    case list => list.map(_._2).mkString(", ")
  }

  private def template: Parser[String] = StartEnd ~ word ~ typeList ~ StartEnd ^^ {
    case _ ~ word ~ args ~ _ => s"$word<$args>"
  }
}

