package tlang.options.arguments

import tlang.formatting.{Formatter, FormattingStyle, FormattingStyles}
import tlang.messages.ErrorStringContext
import tlang.options.ArgumentFlag
import tlang.utils.Extensions._

case object FormattingStyleFlag extends ArgumentFlag[FormattingStyle] {
  override val name           = "formatting"
  override val argDescription = "style"

  override def description(formatter: Formatter): String = {
    import formatter.formatting._
    s"""
       |Chooses the formatting style of messages produced by the tlang.compiler.
       |'${ Blue(FormattingStyles.Ascii.styleName) }' will only produce ASCII-characters.
       |Type --${ Magenta(CompilerHelpFlag.name) } ${ Magenta(name) } for more information.
      """.stripMargin.trim
  }

  override def extendedDescription(formatter: Formatter): String = {
    val formatting = formatter.formatting
    import formatting._

    val boxNames = formatting.makeList(FormattingStyles.Names.map(Blue))
    val desc =
      s"""|The --${ Magenta(name) } flag determines what style to use for all output produced by the T compiler.
          |The style '${ Blue(FormattingStyles.Ascii.styleName) }' only produces ASCII-characters which can be useful when the T compiler is ran on simpler terminals.
          |The default formatting style is '${ Blue(FormattingStyles.DefaultFormatting.styleName) }'.
          |
          |The following styles are available:
          |
          |$boxNames
          |
          |The different formatting styles look like this:
          |""".stripMargin.trim

    desc + System.lineSeparator * 2 + formatBoxes(formatter)
  }


  override protected def verifyArgument(formatting: String)(implicit errorContext: ErrorStringContext): Unit = {
    import errorContext.ErrorStringContext

    if (formatting.toLowerCase notIn FormattingStyles.Names.map(_.toLowerCase)) {
      val suggestion = errorContext.suggestion(formatting, FormattingStyles.Names)
      error(err"$formatting is not a valid formatting style.$suggestion")
    }
  }


  override def parseValue(args: Set[String]): FormattingStyle = {
    args.headOption
      .flatMap(formatting => FormattingStyles.find(_.styleName.toLowerCase == formatting.toLowerCase))
      .getOrElse(FormattingStyles.DefaultFormatting)
  }

  private def formatBoxes(formatter: Formatter): String = {
    val formatting = formatter.formatting
    import formatting._
    val exampleFormatting = formatting.copy(lineWidth = formatting.lineWidth - 4)

    val lorumIpsum =
      s"""|Lorem ipsum dolor sit amet, consectetur ${ Red("adipiscing") } elit. Vestibulum massa augue,
          |${ Magenta("dictum") } eget metus ac, bibendum ${ Yellow("ultricies") } ligula.
          |Aliquam ${ Green("commodo") } ante vitae tellus pharetra dignissim. ${ Cyan("Suspendisse") } non arcu
          |vitae ligula ${ Blue("varius") } suscipit. Etiam tincidunt pretium est, auctor ${ Red("congue") } est
          |laoreet et. Sed ${ Blue("congue") } eu semut sodales.
        """.stripMargin.trim

    FormattingStyles
      .map { style =>
        val format = exampleFormatting.copy(formattingStyle = style)
        val list = s"A ${ Cyan("list") }" + System.lineSeparator + s"${ format.makeList(Red("A"), Green("B"), Blue("C")) }"
        val column = s"A ${ Yellow("column") }."
        val boxFormatter = formatter.copy(formatting = format)
        boxFormatter
          .grid
          .trim(false)
          .header(Bold(Blue(style.styleName)))
          .row()
          .content(lorumIpsum)
          .row(3)
          .content(list, column, lorumIpsum)
          .render()
      }
      .mkString(System.lineSeparator * 2)
  }


}