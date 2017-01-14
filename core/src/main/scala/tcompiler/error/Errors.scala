package tcompiler.error

import tcompiler.imports.ImportMap
import tcompiler.utils.{Context, Positioned}

import scala.util.matching.Regex

/**
  * Created by Tim Lindeberg on 5/13/2016.
  */
trait Errors {

  val ErrorLetters: String
  var ctx         : Context
  var importMap   : ImportMap

  val QuoteRegex: Regex = """'(.+?)'""".r

  def warning(errorCode: Int, msg: String, pos: Positioned): Unit =
    report(errorCode, msg, ErrorLevel.Warning, pos)

  def fatal(errorCode: Int, msg: String, pos: Positioned): Nothing = {
    report(errorCode, msg, ErrorLevel.Fatal, pos)
    throw new Exception // Won't happen but is needed for type Nothing
  }

  def report(errorCode: Int, msg: String, errorLevel: ErrorLevel, pos: Positioned): Unit = {

    val names = QuoteRegex.findAllIn(msg.toString).map { name =>
      name -> importMap.getErrorName(name)
    }.toMap

    val err = Error(ErrorLetters, errorCode, msg, errorLevel, pos, names)
    ctx.reporter.report(err)
  }

}

object Errors {
  val ErrorName = "$ERROR$"
}

case class Error(letters: String, code: Int, msg: Any, errorLevel: ErrorLevel, pos: Positioned, names: Map[String, String]) {
  override def equals(obj: scala.Any): Boolean = obj match {
    case Error(letters0, code0, msg0, errorLevel0, pos0, names0) =>
      letters == letters0 && code == code0 && msg0 == msg0 && errorLevel == errorLevel0 && pos.equalPos(pos0)
    case _                                                       => false
  }
}

trait ErrorLevel
object ErrorLevel {
  case object Warning extends ErrorLevel
  case object Error extends ErrorLevel
  case object Fatal extends ErrorLevel
}

