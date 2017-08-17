package tlang.compiler.options

import tlang.compiler.Main
import tlang.compiler.code.Lowering
import tlang.utils.Enumeration
import tlang.utils.Extensions._
import tlang.utils.formatting.BoxStyles.{Ascii, BoxStyle}
import tlang.utils.formatting.grid.Grid
import tlang.utils.formatting.{BoxStyles, Colors, Formatter, Formatting}

object Flags {

  sealed abstract class Flag extends Ordered[Flag] with Product with Serializable {

    val flag: String
    val shortFlag: Option[String] = None
    def description(formatter: Formatter): String
    def extendedDescription(formatter: Formatter): String = description(formatter).stripMargin.trim

    def flagName(formatting: Formatting): String = {
      import formatting._
      val shortFlagDescription = shortFlag.map(f => s" (-${ Magenta(f) })").getOrElse("")
      "--" + Magenta(flag) + shortFlagDescription
    }

    def matchesString(str: String): Boolean = {
      val lower = str.toLowerCase
      val res = lower == s"--$flag" || shortFlag.exists(flag => s"-$flag" == lower)
      res
    }

    def compare(that: Flag): Int = flag.length - that.flag.length
    def unapply(str: String): Boolean = matchesString(str)

  }


  sealed abstract class BooleanFlag extends Flag {
    def defaultValue: Boolean = false
  }

  sealed abstract class ArgumentFlag extends Flag {

    val arg: String

    override def flagName(formatting: Formatting): String = {
      import formatting._
      // Dropping space
      super.flagName(formatting) + s" <${ Blue(arg) }> "
    }

  }


  sealed abstract class NumberFlag extends ArgumentFlag {

    val arg: String = "num"
    val defaultValue: Int

  }

  sealed abstract class OptionalArgumentFlag extends ArgumentFlag {
    val defaultArg: String
    def isValidArg(arg: String): Boolean
  }

  sealed abstract class JsonFlag extends ArgumentFlag

  //--------------------------------------------------------------------------------
  //-- Flags
  //--------------------------------------------------------------------------------

  case object Exec extends BooleanFlag {
    override val flag = "exec"

    override def description(formatter: Formatter): String = {
      "Executes the program after compilation if it contains a main class."
    }

  }

  case object SuppressWarnings extends BooleanFlag {
    override val flag = "nowarn"

    override def description(formatter: Formatter): String = {
      "Suppresses warning messages."
    }

  }

  case object PrintOutput extends OptionalArgumentFlag {
    override val flag               = "printoutput"
    override val arg                = "phase"
    override val defaultArg: String = Lowering.phaseName

    override def isValidArg(arg: String): Boolean = arg.toLowerCase in validArgs

    private val validArgs = Main.CompilerPhases.map(_.phaseName)

    override def description(formatter: Formatter): String = {
      import formatter.formatting._
      s"""
         |Prints the output after a given compiler phase.
         |If no argument is given the code is printed as it looks before the final code is generated (after the lowering phase).
         |Type --${ Magenta(Help.flag) } ${ Magenta(flag) } for more information.
      """.stripMargin.trim
    }


    override def extendedDescription(formatter: Formatter): String = {
      import formatter.formatting._

      val phases = Main.CompilerPhases.map(phase => "  " + Blue(phase.phaseName.capitalize)).mkString("\n")
      s"""|The --${ Magenta(flag) } flag prints the output after a given compiler phase.
          |This can be the tokens produced by the Lexer, a pretty printed and formatted AST or the
          |byte code generated by the Codegenerating phase.
          |The compiler phase are executed in the following order:
          |
          |$phases
          |
          |All these phase can be used as arguments.
          |If no argument is given the code is printed as it looks before the final code is generated (after the Lowering phase).
          |
          |Example: --${ Magenta(flag) } ${ Blue("lexer") },${ Blue("desugaring") },${ Blue("codegeneration") }
          |""".stripMargin.trim
    }

  }

  case object Verbose extends BooleanFlag {
    override val flag      = "verbose"
    override val shortFlag = Some("v")

    override def description(formatter: Formatter): String = {
      """
        |Prints additional information during compilation such as elapsed time
        |for each compilation phase.
      """.stripMargin.trim
    }

  }

  case object Help extends OptionalArgumentFlag {
    override val flag       = "help"
    override val shortFlag  = Some("h")
    override val arg        = "about"
    override val defaultArg = ""

    override def isValidArg(arg: String): Boolean = arg.toLowerCase in ValidArgs

    lazy val ValidArgs: List[String] =
      Flag.flagNames.map(_.replaceAll("\\-", "")).filter(!matchesString(_))

    override def description(formatter: Formatter): String = {
      s"""
         |Prints help information and exits. Giving a flag as argument will give more information about that flag.
      """.stripMargin.trim
    }

  }

  case object Directory extends ArgumentFlag {
    override val flag      = "directory"
    override val shortFlag = Some("d")
    override val arg       = "dir"

    override def description(formatter: Formatter): String = {
      "Specify a path where generated classes are placed."
    }
  }

  case object Version extends BooleanFlag {
    override val flag = "version"

    override def description(formatter: Formatter): String = {
      "Prints version information and exits."
    }
  }

  case object WarningIsError extends BooleanFlag {
    override val flag = "werror"

    override def description(formatter: Formatter): String = {
      "Treats warnings as errors and exits compilation."
    }
  }

  case object NoColor extends BooleanFlag {
    override val flag = "no-color"

    override def description(formatter: Formatter): String = {
      import formatter.formatting._
      s"Removes all ${ Red }c${ Green }o${ Blue }l${ Yellow }o${ Magenta }r${ Reset } from the output."
    }
  }

  case object FormattingStyle extends ArgumentFlag {
    override val flag = "formatting"
    override val arg  = "style"

    override def description(formatter: Formatter): String = {
      import formatter.formatting._
      s"""
         |Chooses the formatting style of messages produced by the tlang.compiler.
         |'${ Blue(BoxStyles.Ascii.styleName) }' will only produce ASCII-characters.
         |Type --${ Magenta(Help.flag) } ${ Magenta(flag) } for more information.
      """.stripMargin.trim
    }


    override def extendedDescription(formatter: Formatter): String = {
      val formatting = formatter.formatting
      import formatting._

      val boxes = BoxStyles.All
      val boxNames = formatting.makeList(boxes.map(box => Blue(box.styleName)))
      val desc =
        s"""|The --${ Magenta(flag) } flag determines what style to use for all output produced by the T compiler.
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

  case object ClassPathFlag extends ArgumentFlag {
    override val flag      = "classpath"
    override val shortFlag = Some("cp")
    override val arg       = "path"

    override def description(formatter: Formatter): String = {
      import formatter.formatting._
      s"Specify a ${ Blue("path") } where classes should be searched for."
    }
  }

  case object MaxErrors extends NumberFlag {
    override val defaultValue = 100
    override val flag         = "maxerrors"

    override def description(formatter: Formatter): String = {
      import formatter.formatting._
      s"""
         |Specify the maximum number of errors to report. The default is '${ Blue(defaultValue) }'.
         |Enter '${ Blue("-1") }' to show all errors.
       """.stripMargin.trim
    }

  }

  case object IgnoreDefaultImports extends ArgumentFlag {
    override val flag = "ignoreimport"
    override val arg  = "import"

    override def description(formatter: Formatter): String = {
      import formatter.formatting._
      s"""
         |Specify a default import to ignore.
         |Example: --${ Magenta(flag) } java::lang::object
      """.stripMargin.trim
    }

  }

  case object ErrorContext extends NumberFlag {
    override val defaultValue = 2

    override val flag      = "errorcontext"
    override val shortFlag = Some("c")
    override val arg       = "num"


    override def description(formatter: Formatter): String = {
      import formatter.formatting._
      s"""
         |Specify how many lines to display around an error position in error messages.
         |The default is '${ Blue(defaultValue) }'.
      """.stripMargin.trim
    }

  }

  case object LineWidth extends NumberFlag {
    override val defaultValue = -1

    val DefaultWidth = 120

    override val flag = "linewidth"
    override val arg  = "num"


    override def description(formatter: Formatter): String = {
      import formatter.formatting._
      s"""
         |Specifies the width of a line in error messages and output.
         |If none is given (or ${ Blue(-1) } is given) the width of the terminal will be used, if it can be determined.
         |Otherwise, '${ Blue(DefaultWidth) }' will be used.
      """.stripMargin.trim
    }

  }

  case object ColorScheme extends JsonFlag {

    override val flag: String = "colorscheme"
    override val arg : String = "colormap"

    override def description(formatter: Formatter): String = {
      import formatter.formatting._
      s"""
         |Define the color scheme to use when printing error messages and code output.
         |Argument is a JSON map of colors, type --${ Magenta(Help.flag) } ${ Magenta(flag) } for more details.
      """.stripMargin.trim
    }


    override def extendedDescription(formatter: Formatter): String = {
      import formatter.formatting._
      import tlang.utils.formatting.Colors.ColorScheme._

      val validKeys = ColorSchemeNames.map("   " + Blue(_)).mkString("\n")
      val validColors = Colors.ColorNames.map("   " + Magenta(_)).mkString("\n")
      s"""
         |The --${ Magenta(flag) } flags accepts a JSON body as it's argument. The following types can be set:
         |
         |$validKeys
         |
         |The types can be set to either an ANSI escape code, an empty string or one of the following:
         |
         |$validColors
         |
         |Example:
         |--${ Magenta(flag) } {
         |  \\"$KeywordName\\":  \\"red\\",
         |  \\"$VariableName\\": \\"\\u001b[1;4;32m\\",
         |  \\"$ClassName\\":    \\"\\",
         |  \\"$MethodName\\":   \\"magenta\\",
         |  \\"$StringName\\":   \\"green\\",
         |  \\"$NumberName\\":   \\"bold\\",
         |  \\"$CommentName\\":  \\"underlined\\",
         |  \\"$SymbolName\\":    \\"black\\"
         |}
         |""".stripMargin.trim
    }
  }

  case object Phases extends BooleanFlag {
    override val flag: String = "phases"
    override def description(formatter: Formatter): String =
      "Prints information about the phases of the T-Compiler and exits."
  }


  // These have to be defined below the Flags, otherwise the macro won't work
  // This code looks like it could be refactored to remove duplication but
  // it won't work with the enumeration macro.
  object BooleanFlag {
    lazy val All: List[BooleanFlag] = Enumeration.instancesOf[BooleanFlag].toList
    def unapply(str: String): Option[BooleanFlag] = All.find(_.matchesString(str))
  }

  object ArgumentFlag {
    lazy val All: List[ArgumentFlag] =
      Enumeration.instancesOf[ArgumentFlag].toList ++ Enumeration.instancesOf[NumberFlag].toList
    def unapply(str: String): Option[ArgumentFlag] = All.find(_.matchesString(str))
  }

  object OptionalArgumentFlag {
    lazy val All: List[OptionalArgumentFlag] = Enumeration.instancesOf[OptionalArgumentFlag].toList
    def unapply(str: String): Option[OptionalArgumentFlag] = All.find(_.matchesString(str))
  }

  object NumberFlag {
    lazy val All: List[NumberFlag] = Enumeration.instancesOf[NumberFlag].toList
    def unapply(str: String): Option[NumberFlag] = All.find(_.matchesString(str))
  }

  object JsonFlag {

    // The enumeration macro doesn't work with only one type. When a new JSON flag is added
    // this should be changed to look like the above objects.
    lazy val All: List[JsonFlag] = List(ColorScheme)
    def unapply(str: String): Option[JsonFlag] = All.find(_.matchesString(str))
  }

  object Flag {
    lazy val All: List[Flag] =
      (JsonFlag.All ++
        OptionalArgumentFlag.All ++
        BooleanFlag.All ++
        ArgumentFlag.All
        ).sortBy(_.flag)

    def flagNames: List[String] = All.flatMap { flag =>
      s"--${ flag.flag }" :: flag.shortFlag.map(f => s"-$f" :: Nil).getOrElse(Nil)
    }

    def get(name: String): Option[Flag] = All.find { flag =>
      val lower = name.toLowerCase
      lower == flag.flag || flag.shortFlag.contains(lower)
    }
  }

}
