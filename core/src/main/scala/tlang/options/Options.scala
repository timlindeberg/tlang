package tlang.options

import tlang.compiler.MainErrors
import tlang.options.Arguments.{Argument, FlagArgument, PositionalArgument}
import tlang.utils.Extensions._

import scala.collection.mutable


case class Options(flags: List[FlagArgument[_]], positionalArgument: Option[PositionalArgument[_]], arguments: Array[String]) extends MainErrors {

  private val argumentValues: mutable.HashMap[Argument[_], Set[String]] = mutable.HashMap()

  def addArgs(argument: Argument[_], args: Set[String]): Unit = {
    val existing = argumentValues.getOrElse(argument, Set[String]())
    argumentValues += argument -> (existing ++ args)
  }

  def apply[F](argument: Argument[F]): F = argument.value(argumentValues.getOrElse(argument, Set()))

  private def processOptions(args: List[String]): Unit = {
    if (args == Nil)
      return

    val rest = flags.findDefined { flag =>
      flag.matches(args) collect { case (argsForFlag, rest) =>
        addArgs(flag, argsForFlag)
        rest
      }
    } getOrElse {
      val arg :: rest = args
      positionalArgument match {
        case Some(default) => addArgs(default, Set(arg))
        case None          => FatalUnrecognizedArgument(arg)
      }
      rest
    }
    processOptions(rest)
  }

  processOptions(arguments.toList)

}
