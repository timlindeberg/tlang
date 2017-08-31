package tlang.options.arguments

import tlang.formatting.Formatter
import tlang.messages.ErrorStringContext
import tlang.options.ArgumentFlag
import tlang.utils.Extensions._

case object ClassPathFlag extends ArgumentFlag[Set[String]] {
  override val name           = "classpath"
  override val shortFlag      = Some("cp")
  override val argDescription = "path"

  override def description(formatter: Formatter): String = {
    import formatter.formatting._
    s"Specify a ${ Blue("path") } where classes should be searched for."
  }

  override def parseValue(args: Set[String]): Set[String] = args

  protected override def verifyArgument(classPath: String)(implicit errorContext: ErrorStringContext): Unit = {
    import errorContext.ErrorStringContext
    if (!classPath.isValidPath)
      error(err"Invalid class path: $classPath.")
  }

  private def ErrorInvalidClassPath(classPath: String)(implicit errorContext: ErrorStringContext): Nothing = {
    import errorContext.ErrorStringContext
    throw new IllegalArgumentException(err"Invalid class path: $classPath.")
  }
}