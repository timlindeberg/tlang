package tlang.compiler.options

import tlang.compiler.Main
import tlang.compiler.code.Desugaring
import tlang.utils.Enumeration
import tlang.utils.Extensions._
import tlang.utils.formatting.Boxes.{Box, Simple}
import tlang.utils.formatting.{Boxes, Colors, Formatting}

object Flags {

  sealed abstract class Flag extends Ordered[Flag] with Product with Serializable {

    val flag: String
    val shortFlag: Option[String] = None
    def description(formatting: Formatting): String
    def extendedDescription(formatting: Formatting): String = description(formatting).stripMargin.trim

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


  sealed abstract class BooleanFlag extends Flag

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

    override def description(formatting: Formatting): String = {
      "Executes the program after compilation if it contains a main class."
    }

  }

  case object SuppressWarnings extends BooleanFlag {
    override val flag = "nowarn"

    override def description(formatting: Formatting): String = {
      "Suppresses warning messages."
    }

  }

  case object PrintOutput extends OptionalArgumentFlag {
    override val flag               = "printoutput"
    override val arg                = "stage"
    override val defaultArg: String = Desugaring.compilerStageName

    override def isValidArg(arg: String): Boolean = arg.toLowerCase in validArgs

    private val validArgs = Main.CompilerStages.map(_.compilerStageName)

    override def description(formatting: Formatting): String = {
      import formatting._
      s"""
         |Prints the output after a given tlang.compiler stage.
         |If no argument is given the code is printed as it looks before the final code is generated.
         |Type --${ Magenta(Help.flag) } ${ Magenta(flag) } for more information.
      """.stripMargin.trim
    }


    override def extendedDescription(formatting: Formatting): String = {
      import formatting._

      val stages = Main.CompilerStages.map(stage => "  " + Blue(stage.compilerStageName.capitalize)).mkString("\n")
      s"""|The --${ Magenta(flag) } flag prints the output after a given tlang.compiler stage.
          |This can be the tokens produced by the Lexer, a pretty printed AST or the byte code generated by the Codegeneration stage.
          |The tlang.compiler stages are executed in the following order:
          |
          |$stages
          |
          |All these stages can be used as arguments.
          |If no argument is given the code is printed as it looks before the final code is generated.
          |
          |Example: --${ Magenta(flag) } ${ Blue("lexer") },${ Blue("desugaring") },${ Blue("codegeneration") }
          |""".stripMargin.trim
    }

  }

  case object Verbose extends BooleanFlag {
    override val flag      = "verbose"
    override val shortFlag = Some("v")

    override def description(formatting: Formatting): String = {
      """
        |Prints additional information during compilation such as elapsed time
        |for each compilation stage.
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

    override def description(formatting: Formatting): String = {
      s"""
         |Prints help information and exits. Giving a flag as argument will give more information about that flag.
      """.stripMargin.trim
    }

  }

  case object Directory extends ArgumentFlag {
    override val flag      = "directory"
    override val shortFlag = Some("d")
    override val arg       = "dir"

    override def description(formatting: Formatting): String = {
      "Specify a path where generated classes are placed."
    }
  }

  case object Version extends BooleanFlag {
    override val flag = "version"

    override def description(formatting: Formatting): String = {
      "Prints version information and exits."
    }
  }

  case object WarningIsError extends BooleanFlag {
    override val flag = "werror"

    override def description(formatting: Formatting): String = {
      "Treats warnings as errors and exits compilation."
    }
  }

  case object Formatting extends ArgumentFlag {
    override val flag = "formatting"
    override val arg  = "style"

    override def description(formatting: Formatting): String = {
      import formatting._
      s"""
         |Chooses the formatting style of messages produced by the tlang.compiler.
         |'${ Blue("Simple") }' will only produce ASCII-characters and use no colors.
         |Type --${ Magenta(Help.flag) } ${ Magenta(flag) } for more information.
      """.stripMargin.trim
    }


    override def extendedDescription(formatting: Formatting): String = {
      import formatting._

      val boxes = Boxes.All.toList
      val boxNames = boxes.map(box => "  " + Blue(box.name)).mkString("\n")
      val desc =
        s"""|The --${ Magenta(flag) } flag determines what style to use for all output produced by the tlang.compiler.
            |The style '${ Blue("Simple") }' only produces ASCII-characters and no colors which can be useful when the tlang.compiler is ran on simpler terminals.
            |The default formatting style is '${ Blue(Boxes.DefaultBox.name) }'.
            |
            |The following styles are available:
            |
            |$boxNames
            |
            |The different formatting styles look like this:
            |""".stripMargin.trim

      desc + "\n\n" + formatBoxes(boxes, formatting)
    }

    private def formatBoxes(boxes: List[Box], formatting: Formatting): String = {
      import formatting._

      val seperator = "  "
      val boxWidth = boxes.map(_.name.length).max + 4
      val perRow = lineWidth / (boxWidth + seperator.length)
      val styles = boxes
        .map { box =>
          val blocks = List("A " + Blue("block") + ".", "Another " + Blue("block") + ".")
          val simple = box == Simple
          val exampleFormatting = tlang.utils.formatting.Formatting(
            box, boxWidth, colorScheme = formatting.colorScheme, useColor = !simple, asciiOnly = simple, trim = false)
          val header = Bold(Magenta(box.name))
          exampleFormatting.makeBox(header, blocks).split("\n").toList
        }
        .grouped(perRow)
        .map { group => group.transpose.map(_.mkString(seperator)).mkString("\n") }
        .mkString("\n")
      center(styles)
    }
  }

  case object ClassPathFlag extends ArgumentFlag {
    override val flag      = "classpath"
    override val shortFlag = Some("cp")
    override val arg       = "path"

    override def description(formatting: Formatting): String = {
      import formatting._
      s"Specify a ${ Blue("path") } where classes should be searched for."
    }
  }

  case object MaxErrors extends NumberFlag {
    override val defaultValue = 100
    override val flag         = "maxerrors"

    override def description(formatting: Formatting): String = {
      import formatting._
      s"""
         |Specify the maximum number of errors to report. The default is '${ Blue(defaultValue) }'.
         |Enter '${ Blue("-1") }' to show all errors.
       """.stripMargin.trim
    }

  }

  case object IgnoreDefaultImports extends ArgumentFlag {
    override val flag = "ignoreimport"
    override val arg  = "import"

    override def description(formatting: Formatting): String = {
      import formatting._
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


    override def description(formatting: Formatting): String = {
      import formatting._
      s"""
         |Specify how many lines to display around an error position in error messages.
         |The default is '${ Blue(defaultValue) }'.
      """.stripMargin.trim
    }

  }

  case object LineWidth extends NumberFlag {
    override val defaultValue = 80

    override val flag = "linewidth"
    override val arg  = "num"


    override def description(formatting: Formatting): String = {
      import formatting._
      s"""
         |Specify the width of a line in error message and output.
         |The default is '${ Blue(defaultValue) }' chars.
      """.stripMargin.trim
    }

  }

  case object ColorScheme extends JsonFlag {

    override val flag: String = "colorscheme"
    override val arg : String = "colormap"

    override def description(formatting: Formatting): String = {
      import formatting._
      s"""
         |Define the color scheme to use when printing error messages and code output.
         |Argument is a JSON map of colors, type --${ Magenta(Help.flag) } ${ Magenta(flag) } for more details.
      """.stripMargin.trim
    }


    override def extendedDescription(formatting: Formatting): String = {
      import formatting._
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
