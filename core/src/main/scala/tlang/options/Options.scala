package tlang.options

import tlang.formatting.{Formatter, Formatting}
import tlang.messages.ErrorStringContext
import tlang.utils.Extensions._

import scala.collection.mutable


object Options {
  def Empty = Options(Nil, None, Array())(ErrorStringContext())
}

case class Options(
  flags: List[FlagArgument[_]],
  positionalArgument: Option[PositionalArgument[_]],
  arguments: Array[String])
  (implicit errorContext: ErrorStringContext) {

  private val argumentValues: mutable.HashMap[Argument[_], Set[String]] = mutable.HashMap()


  def apply[F](argument: Argument[F]): F = {
    val valuesForArgument = argumentValues.getOrElse(argument, Set())
    argument.parseValue(valuesForArgument)
  }

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
      if (arg.startsWith("-"))
        ErrorInvalidFlag(arg)

      positionalArgument match {
        case Some(positional) =>
          if (arg.nonEmpty) {
            positional.verifyArgument(arg)
            addArg(positional, arg)
          }
        case None             => ErrorUnrecognizedArgument(arg)
      }
      rest
    }

    processOptions(rest)
  }

  processOptions(arguments.toList)

  private def addArg(argument: Argument[_], args: String): Unit = addArgs(argument, Set(args))
  private def addArgs(argument: Argument[_], args: Set[String]): Unit = {
    val existing = argumentValues.getOrElse(argument, Set[String]())
    argumentValues += argument -> (existing ++ args)
  }


  private def ErrorUnrecognizedArgument(arg: String) = {
    import errorContext.ErrorStringContext
    throw new IllegalArgumentException(err"$arg is not a valid argument.")
  }

  private def ErrorInvalidFlag(arg: String) = {
    import errorContext.ErrorStringContext
    val suggestion = errorContext.suggestion(arg, flags.flatMap(_.names))
    throw new IllegalArgumentException(
      err"$arg is not a valid flag.$suggestion Type '--help' to see a list of valid commands."
    )
  }

}


trait Argument[T] {

  def parseValue(args: Set[String]): T
  def error(message: String): Nothing = throw new IllegalArgumentException(message)

}

trait PositionalArgument[T] extends Argument[T] {

  def verifyArgument(arg: String)(implicit errorContext: ErrorStringContext): Unit = {}

}

trait FlagArgument[T] extends Argument[T] {

  def name: String
  def shortFlag: Option[String] = None
  def description(formatter: Formatter): String
  def extendedDescription(formatter: Formatter): String = description(formatter).stripMargin.trim

  def parseValue(args: Set[String]): T

  def matches(args: List[String])(implicit errorContext: ErrorStringContext): Option[(Set[String], List[String])]

  def flagName(formatting: Formatting): String = {
    import formatting._
    val shortFlagDescription = shortFlag.map(f => s" (-${ Magenta(f) })").getOrElse("")
    "--" + Magenta(name) + shortFlagDescription
  }

  def names: List[String] = s"--$name" :: shortFlag.map(short => List(s"-$short")).getOrElse(Nil)

  def matchesString(str: String): Boolean = str.toLowerCase in names

}

trait BooleanFlag extends FlagArgument[Boolean] {

  private val Active = "ACTIVE"

  override def parseValue(args: Set[String]): Boolean = args.contains(Active)
  override def matches(args: List[String])(implicit errorContext: ErrorStringContext): Option[(Set[String], List[String])] = args match {
    case flag :: rest if matchesString(flag) => Some(Set(Active), rest)
    case _                                   => None
  }
}

trait ArgumentFlag[T] extends FlagArgument[T] {

  def argDescription: String

  override def flagName(formatting: Formatting): String = {
    import formatting._
    // Dropping space
    super.flagName(formatting) + s" <${ Blue(argDescription) }> "
  }


  override def matches(args: List[String])(implicit errorContext: ErrorStringContext): Option[(Set[String], List[String])] = args match {
    case flag :: arg :: rest if matchesString(flag) => Some(getArgs(arg), rest)
    case _                                          => None
  }

  protected def getArgs(arg: String)(implicit errorContext: ErrorStringContext): Set[String] =
    arg
      .split(",")
      .map(_.trim)
      .filter(_.nonEmpty)
      .use { args => args foreach verifyArgument }
      .toSet


  protected def verifyArgument(arg: String)(implicit errorContext: ErrorStringContext): Unit = {}

}

trait OptionalArgumentFlag[T] extends ArgumentFlag[T] {
  def defaultArg: String
  def isValidArg(arg: String): Boolean

  override def matches(args: List[String])(implicit errorContext: ErrorStringContext): Option[(Set[String], List[String])] = args match {
    case flag :: rest if matchesString(flag) =>
      rest match {
        case maybeArg :: rest =>
          val args = getArgs(maybeArg)
          if (args forall isValidArg)
            Some((args, rest))
          else
            Some((Set(defaultArg), maybeArg :: rest))
        case Nil              => Some(Set(defaultArg), Nil)
      }
    case _                                   => None
  }

}

trait DictionaryFlag[T] extends ArgumentFlag[T] {

  def parseValue(args: Map[String, String]): T

  override def parseValue(args: Set[String]): T = {
    val map = mutable.Map[String, String]()
    args.foreach { keyValue =>
      val Array(key, value) = keyValue.split("=")
      map += key.toLowerCase -> value
    }

    parseValue(map.toMap)
  }

  private val KeyValueRegex = """.+=.+""".r

  protected def verifyArg(key: String, value: String)(implicit errorContext: ErrorStringContext): Unit = {}

  protected override def verifyArgument(keyValue: String)(implicit errorContext: ErrorStringContext): Unit = {
    import errorContext.ErrorStringContext

    if (!KeyValueRegex.matches(keyValue))
      error(err"The argument following the flag $name is not a valid key value pair: $keyValue. An argument should have the form A=B,C=D,E=F.")

    val Array(key, value) = keyValue.split("=")
    verifyArg(key, value)
  }
}

trait NumberFlag extends ArgumentFlag[Int] {

  def defaultValue: Int

  override val argDescription: String = "num"

  override def parseValue(args: Set[String]): Int = {
    if (args.isEmpty)
      return defaultValue

    // Since the arguments is a set we don't the order the numbers were added.
    // We have to pick one of the values so we choose the largest one
    args.map { _.toInt }.max
  }

  protected override def verifyArgument(arg: String)(implicit errorContext: ErrorStringContext): Unit = {
    import errorContext.ErrorStringContext

    if (!arg.isNumber)
      error(err"The argument $arg is not a valid number.")
  }

}
