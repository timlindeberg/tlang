package tlang.options.arguments

import tlang.compiler.error.ErrorStringContext
import tlang.formatting.BoxStyles.{Ascii, BoxStyle}
import tlang.formatting.grid.Grid
import tlang.formatting.{BoxStyles, Formatter}
import tlang.options.ArgumentFlag
import tlang.utils.Extensions._

case object FormattingStyleFlag extends ArgumentFlag[BoxStyle] {
  override val name           = "formatting"
  override val argDescription = "style"

  override def description(formatter: Formatter): String = {
    import formatter.formatting._
    s"""
       |Chooses the formatting style of messages produced by the tlang.compiler.
       |'${ Blue(BoxStyles.Ascii.styleName) }' will only produce ASCII-characters.
       |Type --${ Magenta(HelpFlag.name) } ${ Magenta(name) } for more information.
      """.stripMargin.trim
  }

  override def extendedDescription(formatter: Formatter): String = {
    val formatting = formatter.formatting
    import formatting._

    val boxes = BoxStyles.All
    val boxNames = formatting.makeList(BoxStyles.Names.map(Blue))
    val desc =
      s"""|The --${ Magenta(name) } flag determines what style to use for all output produced by the T compiler.
          |The style '${ Blue(BoxStyles.Ascii.styleName) }' only produces ASCII-characters which can be useful when the T compiler is ran on simpler terminals.
          |The default formatting style is '${ Blue(BoxStyles.DefaultBox.styleName) }'.
          |
          |The following styles are available:
          |
          |$boxNames
          |
          |The different formatting styles look like this:
          |""".stripMargin.trim

    desc + "\n\n" + formatBoxes(boxes, formatter)
  }


  override protected def verifyArgument(formatting: String)(implicit errorContext: ErrorStringContext): Unit = {
    import errorContext.ErrorStringContext

    if (formatting.toLowerCase notIn BoxStyles.Names.map(_.toLowerCase)) {
      val suggestion = errorContext.suggestion(formatting, BoxStyles.Names)
      error(err"$formatting is not a valid formatting style.$suggestion")
    }
  }


  override def parseValue(args: Set[String]): BoxStyle = {
    args.headOption
      .flatMap(formatting => BoxStyles.All.find(_.styleName.toLowerCase == formatting.toLowerCase))
      .getOrElse(BoxStyles.DefaultBox)
  }

  private def formatBoxes(boxStyles: List[BoxStyle], formatter: Formatter): String = {
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

    boxStyles
      .map { style =>
        val format = exampleFormatting.copy(boxStyle = style, asciiOnly = style == Ascii)
        val list = s"A ${ Cyan("list") }\n${ format.makeList(Red("A"), Green("B"), Blue("C")) }"
        val column = s"A ${ Yellow("column") }."
        Grid(formatter.copy(formatting = format))
          .trim(false)
          .header(Bold(Blue(style.styleName)))
          .row()
          .content(lorumIpsum)
          .row(3)
          .content(list, column, lorumIpsum)
          .render()
      }
      .mkString("\n\n")
  }


}