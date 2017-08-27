package tlang.options.arguments

import tlang.compiler.error.ErrorStringContext
import tlang.formatting.Colors.{ColorNameMap, ColorScheme}
import tlang.formatting.{Colors, Formatter}
import tlang.options.DictionaryFlag
import tlang.utils.Extensions._


case object ColorSchemeFlag extends DictionaryFlag[ColorScheme] {

  override val name          : String = "colorscheme"
  override val argDescription: String = "colormap"

  override def description(formatter: Formatter): String = {
    import formatter.formatting._
    s"""
       |Define the color scheme to use when printing error messages and code output.
       |Argument is a JSON map of colors, type --${ Magenta(HelpFlag.name) } ${ Magenta(name) } for more details.
      """.stripMargin.trim
  }


  override def extendedDescription(formatter: Formatter): String = {
    val formatting = formatter.formatting
    import formatting._
    import tlang.formatting.Colors.ColorScheme._

    val validKeys = formatting.makeList(Keys.map(Blue))
    val validColors = formatting.makeList(Colors.ColorNames.map(Magenta))
    s"""
       |The --${ Magenta(name) } flag accepts key-value-pairs as it's argument. The following types can be set:
       |
       |$validKeys
       |
       |The types can be set to either an ANSI escape code or one of the following:
       |
       |$validColors
       |
       |Example:
       |
       |--${ Magenta(name) } $KeywordName=red,$VariableName=1,$ClassName=4,$MethodName=magenta,$StringName=green,$NumberName=bold,$CommentName=underlined,$SymbolName=black
       |""".stripMargin.trim
  }

  override def verifyArg(colorKey: String, color: String)(implicit errorContext: ErrorStringContext): Unit = {
    import errorContext.ErrorStringContext

    if (colorKey.toLowerCase notIn ColorScheme.Keys) {
      val suggestion = errorContext.suggestion(colorKey, ColorScheme.Keys)
      error(err"$colorKey is not a valid part of color scheme.${ suggestion }See --help colorscheme for more information.")
    }

    if (getColorValue(color).isEmpty) {
      val suggestion = errorContext.suggestion(color, Colors.ColorNames)
      error(err"$color is not a color.${ suggestion }See --help colorscheme for more information.")
    }
  }

  override def parseValue(args: Map[String, String]): ColorScheme = {
    import ColorScheme._
    if (args.isEmpty)
      return DefaultColorScheme

    val colors = getColorValues(args)

    new ColorScheme {
      override val Keyword : Int = colors(KeywordName)
      override val Variable: Int = colors(VariableName)
      override val Class   : Int = colors(ClassName)
      override val Method  : Int = colors(MethodName)
      override val String  : Int = colors(StringName)
      override val Number  : Int = colors(NumberName)
      override val Comment : Int = colors(CommentName)
      override val Symbol  : Int = colors(SymbolName)
    }
  }

  private def getColorValues(args: Map[String, String]): Map[String, Int] = {
    ColorScheme.Keys.map { colorKey =>
      colorKey -> args.get(colorKey).flatMap(getColorValue).getOrElse(Colors.NO_COLOR)
    }.toMap
  }

  private def getColorValue(color: String): Option[Int] = {
    if (color.isEmpty)
      return Some(Colors.NO_COLOR)

    if (color.isNumber)
      return Some(color.toInt)

    ColorNameMap.get(color)
  }

}