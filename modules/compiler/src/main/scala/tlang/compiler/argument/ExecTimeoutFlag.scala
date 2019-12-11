package tlang.compiler.argument

import java.util.concurrent.TimeUnit

import tlang.formatting.{ErrorStringContext, Formatter}
import tlang.options.ArgumentFlag

import scala.concurrent.duration.Duration
import scala.util.{Success, Try}

case object ExecTimeoutFlag extends ArgumentFlag[Duration] {
  override val name = "exectimeout"

  override def argDescription: String = "timeout"
  override def description(implicit formatter: Formatter): String = {
    s"""
       |The timeout duration in seconds to be used when executing a program.
       |The execution will cancel once the timout is reached. If ${ highlight(-1) } is passed
       |the program will never timeout.
       |
       |Example: ${ flag(name) } ${ highlight(2.5) } will timeout after ${ highlight(2.5) } seconds
      """
  }

  protected override def verify(arg: String)(implicit errorContext: ErrorStringContext): Unit = {
    import errorContext.ErrorStringContext
    val isValid = Try(arg.toDouble) match {
      case Success(value) => value >= -1
      case _              => false
    }
    if (!isValid) {
      error(err"$arg is not a proper execution time length.")
    }
  }

  override def parseValue(args: Set[String]): Duration = {
    val n = if (args.isEmpty) -1 else args.map { _.toDouble }.max
    if (n == -1) Duration.Inf else Duration(n, TimeUnit.SECONDS)
  }
}
