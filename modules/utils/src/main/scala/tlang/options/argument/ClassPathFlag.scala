package tlang
package options
package argument

import tlang.formatting.{ErrorStringContext, Formatter}

case object ClassPathFlag extends ArgumentFlag[Set[String]] {
  override val name = "classpath"
  override val shortFlag = Some("cp")
  override val argDescription = "path"

  override def description(implicit formatter: Formatter): String =
    s"Specify a ${ highlight("path") } where classes should be searched for."

  override def parseValue(args: Set[String]): Set[String] = args

  protected override def verify(classPath: String)(implicit errorContext: ErrorStringContext): Unit = {
    import errorContext.ErrorStringContext
    if (!classPath.isValidPath)
      error(err"Invalid class path: $classPath.")
  }
}
