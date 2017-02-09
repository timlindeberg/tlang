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


    val code = ErrorLetters + errorLevel.num + leftPadCode(errorCode)
    val err = Error(code, msg, errorLevel, pos, names)
    ctx.reporter.report(err)
  }

  private def leftPadCode(num: Int): String = num match {
    case x if x >= 0 && x < 10     => "00" + x
    case x if x >= 10 && x < 100   => "0" + x
    case x if x >= 100 && x < 1000 => "" + x
  }

}

object Errors {
  val ErrorName = "$ERROR$"
}

case class Error(code: String, msg: Any, errorLevel: ErrorLevel, pos: Positioned, names: Map[String, String]) {
  override def equals(obj: scala.Any): Boolean = obj match {
    case Error(code0, msg0, errorLevel0, pos0, _) =>
      code == code0 && msg0 == msg0 && errorLevel == errorLevel0 && pos.equalPos(pos0)
    case _                                        => false
  }
}

trait ErrorLevel {
  def num: Int
}

object ErrorLevel {
  case object Warning extends ErrorLevel {val num = 1}
  case object Error extends ErrorLevel {val num = 2}
  case object Fatal extends ErrorLevel {val num = 3}
}

