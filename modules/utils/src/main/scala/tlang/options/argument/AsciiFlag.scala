package tlang.options.argument

import tlang.formatting.Formatter
import tlang.options.BooleanFlag
import tlang.utils.Extensions._

case object AsciiFlag extends BooleanFlag {
  override val name = "ascii"

  override def description(implicit formatter: Formatter): String =
    s"""
       |Prints all output from the compiler using only ASCII characters. No Unicode characters will be printed.
       |Type ${ flag(HelpFlag.Name) } ${ highlight(name) } for more information.
      """

  override def extendedDescription(implicit formatter: Formatter): String =
    s"""
       |Prints all output from the compiler using only ASCII characters. No Unicode characters will be printed.
       |
       |This is what the output look like:
       |
       |$formatBoxes
       |"""

  private def formatBoxes(implicit formatter: Formatter): String = {
    val formatting = formatter.formatting
    import formatting._
    val exampleFormatting = formatting.copy(lineWidth = formatting.lineWidth - 4)

    val lorumIpsum =
      s"""
         |Lorem ipsum dolor sit amet, consectetur ${ Red("adipiscing") } elit. Vestibulum massa augue,
         |${ Magenta("dictum") } eget metus ac, bibendum ${ Yellow("ultricies") } ligula.
         |Aliquam ${ Green("commodo") } ante vitae tellus pharetra dignissim. ${ Cyan("Suspendisse") } non arcu
         |vitae ligula ${ Blue("varius") } suscipit. Etiam tincidunt pretium est, auctor ${ Red("congue") } est
         |laoreet et. Sed ${ Blue("congue") } eu semut sodales.
        """.stripMargin.trim

    List(true, false)
      .map { asciiOnly =>
        val boxFormatter = formatter.copy(formatting = exampleFormatting.copy(asciiOnly = asciiOnly))
        val list = s"A ${ Cyan("list") }" + NL + s"${ boxFormatter.list(Red("A"), Green("B"), Blue("C")) }"
        val column = s"A ${ Yellow("column") }."
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
      .mkString(NL * 2)
  }


}
