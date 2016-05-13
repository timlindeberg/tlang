package tcompiler.utils

/**
  * Created by Tim Lindeberg on 5/13/2016.
  */
trait Errors {

  val ErrorPrefix: String
  var ctx        : Context

  def warning(errorCode: Int, msg: String, pos: Positioned = NoPosition) =
    ctx.reporter.error(ErrorPrefix, errorCode, msg, pos)

  def fatal(errorCode: Int, msg: String, pos: Positioned = NoPosition) =
    ctx.reporter.fatal(ErrorPrefix, errorCode, msg, pos)

}
