package tlang.options.arguments

import tlang.formatting.{Formatter, Formatting}
import tlang.options.BooleanFlag

case object AsciiFlag extends BooleanFlag {
  override val name = "ascii"

  override def description(formatter: Formatter): String = {
    import formatter.formatting._
    s"""|Prints all output from the compiler using only ASCII characters.
        |No Unicode characters will be printed.
        |Type --${ Magenta(CompilerHelpFlag.name) } ${ Magenta(name) } for more information.
      """.stripMargin.trim
  }

  override def extendedDescription(formatter: Formatter): String = {
    val desc =
      s"""|Prints all output from the compiler using only ASCII characters.
          |No Unicode characters will be printed.
          |
          |This is what the output would look like:
          |""".stripMargin.trim

    desc + System.lineSeparator * 2 + formatBoxes(formatter)
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

    List(true, false)
      .map { asciiOnly =>
        val list = s"A ${ Cyan("list") }" + System.lineSeparator + s"${ formatter.list(Red("A"), Green("B"), Blue("C")) }"
        val column = s"A ${ Yellow("column") }."
        val boxFormatter = formatter.copy(formatting = Formatting(asciiOnly = asciiOnly))
        val header = if (asciiOnly) "ASCII" else "Unicode"
        boxFormatter
          .grid
          .trim(false)
          .header(Bold(Blue(header)))
          .row()
          .content(lorumIpsum)
          .row(3)
          .content(list, column, lorumIpsum)
          .render()
      }
      .mkString(System.lineSeparator * 2)
  }


}