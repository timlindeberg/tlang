package tlang.options

import java.io.File

import tlang.Constants
import tlang.compiler.code.Lowering
import tlang.compiler.{Main, MainErrors}
import tlang.formatting.BoxStyles.{Ascii, BoxStyle}
import tlang.formatting.Colors.ColorScheme
import tlang.formatting.grid.Grid
import tlang.formatting.{BoxStyles, Colors, Formatter, Formatting}
import tlang.utils.Extensions._

import scala.tools.jline.TerminalFactory
import scala.util.parsing.json.JSON

object Arguments extends MainErrors {

  trait Argument[T] {
    def value(args: Set[String]): T
  }

  trait PositionalArgument[T] extends Argument[T]

  trait FlagArgument[T] extends Argument[T] {

    def flag: String
    def shortFlag: Option[String] = None
    def description(formatter: Formatter): String
    def extendedDescription(formatter: Formatter): String = description(formatter).stripMargin.trim

    def value(args: Set[String]): T

    def matches(args: List[String]): Option[(Set[String], List[String])]

    def flagName(formatting: Formatting): String = {
      import formatting._
      val shortFlagDescription = shortFlag.map(f => s" (-${ Magenta(f) })").getOrElse("")
      "--" + Magenta(flag) + shortFlagDescription
    }

    def matchesString(str: String): Boolean = {
      val lower = str.toLowerCase
      lower == s"--$flag" || shortFlag.exists(flag => s"-$flag" == lower)
    }

  }

  trait BooleanFlag extends FlagArgument[Boolean] {

    private val Active = "ACTIVE"

    override def value(args: Set[String]): Boolean = args.contains(Active)
    override def matches(args: List[String]): Option[(Set[String], List[String])] = args match {
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


    override def matches(args: List[String]): Option[(Set[String], List[String])] = args match {
      case flag :: arg :: rest if matchesString(flag) => Some(getArgs(arg), rest)
      case _                                          => None
    }

    protected def getArgs(arg: String): Set[String] = arg.split(", *").map(_.trim.toLowerCase).filter(_.nonEmpty).toSet

  }

  trait OptionalArgumentFlag[T] extends ArgumentFlag[T] {
    def defaultArg: String
    def isValidArg(arg: String): Boolean

    override def matches(args: List[String]): Option[(Set[String], List[String])] = args match {
      case flag :: rest if matchesString(flag) =>
        rest match {
          case maybeArg :: rest =>
            val args = getArgs(maybeArg)
            if (args.exists(isValidArg))
              Some((args, rest))
            else
              Some((Set(defaultArg), maybeArg :: rest))
          case Nil              => Some(Set(defaultArg), Nil)
        }
      case _                                   => None
    }

  }

  trait JsonFlag[T] extends ArgumentFlag[T] {
    override def matches(args: List[String]): Option[(Set[String], List[String])] = args match {
      case flag :: rest if matchesString(flag) =>
        val restString = rest.mkString(" ")
        val jsonStart = restString.indexOf("{")
        val jsonEnd = restString.lastIndexOf("}")
        if (jsonStart == -1 || jsonEnd == -1)
          FatalInvalidJsonArgument(this, restString)

        val json = restString.substring(jsonStart, jsonEnd + 1)
        val afterJson = restString.substring(jsonEnd + 1).trim
        Some(Set(json), afterJson.split(" ").toList)
      case _                                   => None
    }
  }

  trait NumberFlag extends ArgumentFlag[Int] {

    def defaultValue: Int

    override val argDescription: String = "num"

    override def value(args: Set[String]): Int = {
      val validNums = args.map { num =>
        try {
          num.toInt
        } catch {
          case _: NumberFormatException => FatalInvalidArgToFlag(this, num, Nil)
        }
      }
      if (validNums.isEmpty) defaultValue else validNums.max
    }

  }

  //--------------------------------------------------------------------------------
  //-- Flags
  //--------------------------------------------------------------------------------

  case object FilesArgument extends PositionalArgument[Set[File]] {

    override def value(args: Set[String]): Set[File] = {
      val files = args.flatMap { path =>
        if (path.startsWith("-"))
          FatalInvalidFlag(path, List())

        val file = new File(path)
        if (file.isDirectory) {
          val tFiles = file.listFiles().filter(_.getName.endsWith(Constants.FileEnding))
          if (tFiles.isEmpty)
            FatalGivenDirectoryContainsNoTFiles(path)

          tFiles.toList
        } else {
          if (!file.getName.endsWith(Constants.FileEnding))
            FatalGivenFileIsNotTFile(path)

          List(file)
        }
      }

      files.filter(!_.exists()).foreach(f => FatalCannotFindFile(f.getPath))
      files
    }
  }

  case object ExecFlag extends BooleanFlag {
    override val flag = "exec"

    override def description(formatter: Formatter): String = {
      "Executes the program after compilation if it contains a main class."
    }

  }

  case object SuppressWarningsFlag extends BooleanFlag {
    override val flag = "nowarn"

    override def description(formatter: Formatter): String = {
      "Suppresses warning messages."
    }

  }

  case object PrintOutputFlag extends OptionalArgumentFlag[Set[String]] {
    override val flag               = "printoutput"
    override val argDescription     = "phase"
    override val defaultArg: String = Lowering.phaseName

    override def isValidArg(arg: String): Boolean = arg.toLowerCase in validArgs

    private val validArgs = Main.CompilerPhases.map(_.phaseName)

    override def description(formatter: Formatter): String = {
      import formatter.formatting._
      s"""
         |Prints the output after a given compiler phase.
         |If no argument is given the code is printed as it looks before the final code is generated (after the lowering phase).
         |Type --${ Magenta(HelpFlag.flag) } ${ Magenta(flag) } for more information.
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

    override def value(args: Set[String]): Set[String] = {
      val validPhases = Main.CompilerPhases.map(_.phaseName)
      args.filter { phase => !(phase in validPhases) }.foreach { phase =>
        FatalInvalidArgToFlag(PrintOutputFlag, phase, validPhases)
      }
      args
    }
  }

  case object VerboseFlag extends BooleanFlag {
    override val flag      = "verbose"
    override val shortFlag = Some("v")

    override def description(formatter: Formatter): String = {
      """
        |Prints additional information during compilation such as elapsed time
        |for each compilation phase.
      """.stripMargin.trim
    }

  }

  case object HelpFlag extends OptionalArgumentFlag[Set[String]] {
    override val flag           = "help"
    override val shortFlag      = Some("h")
    override val argDescription = "about"
    override val defaultArg     = "all"

    override def isValidArg(arg: String): Boolean = true

    override def description(formatter: Formatter): String = {
      s"""
         |Prints help information and exits. Giving a flag as argument will give more information about that flag.
      """.stripMargin.trim
    }

    override def value(args: Set[String]): Set[String] = args

  }

  case object DirectoryFlag extends ArgumentFlag[Set[File]] {
    override val flag           = "directory"
    override val shortFlag      = Some("d")
    override val argDescription = "dir"

    override def description(formatter: Formatter): String = {
      "Specify a path where generated classes are placed."
    }

    override def value(args: Set[String]): Set[File] = {
      if (args.isEmpty)
        return Set(new File("."))

      args.filter(!_.isValidPath).foreach(FatalInvalidOutputDirectory)

      args.map { new File(_) }
    }


  }

  case object VersionFlag extends BooleanFlag {
    override val flag = "version"

    override def description(formatter: Formatter): String = {
      "Prints version information and exits."
    }
  }

  case object WarningIsErrorFlag extends BooleanFlag {
    override val flag = "werror"

    override def description(formatter: Formatter): String = {
      "Treats warnings as errors and exits compilation."
    }
  }

  case object NoColorFlag extends BooleanFlag {
    override val flag = "no-color"

    override def description(formatter: Formatter): String = {
      import formatter.formatting._
      s"Removes all $Bold${ Red }c${ Green }o${ Blue }l${ Yellow }o${ Magenta }r${ Reset } from the output."
    }
  }

  case object FormattingStyleFlag extends ArgumentFlag[BoxStyle] {
    override val flag           = "formatting"
    override val argDescription = "style"

    override def description(formatter: Formatter): String = {
      import formatter.formatting._
      s"""
         |Chooses the formatting style of messages produced by the tlang.compiler.
         |'${ Blue(BoxStyles.Ascii.styleName) }' will only produce ASCII-characters.
         |Type --${ Magenta(HelpFlag.flag) } ${ Magenta(flag) } for more information.
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

    private lazy val boxNames = BoxStyles.All.map(_.styleName.toLowerCase)

    override def value(args: Set[String]): BoxStyle = {
      args.foreach { formatting =>
        if (!(formatting in boxNames))
          FatalInvalidArgToFlag(Arguments.FormattingStyleFlag, formatting, boxNames)
      }
      args.headOption
        .flatMap(formatting => BoxStyles.All.find(_.styleName.toLowerCase == formatting))
        .getOrElse(BoxStyles.DefaultBox)
    }
  }

  case object ClassPathFlag extends ArgumentFlag[Set[String]] {
    override val flag           = "classpath"
    override val shortFlag      = Some("cp")
    override val argDescription = "path"

    override def description(formatter: Formatter): String = {
      import formatter.formatting._
      s"Specify a ${ Blue("path") } where classes should be searched for."
    }
    override def value(args: Set[String]): Set[String] = {
      args.filter(!_.isValidPath).foreach(FatalInvalidClassPath)
      args
    }
  }

  case object MaxErrorsFlag extends NumberFlag {
    override val defaultValue = 100

    override def description(formatter: Formatter): String = {
      import formatter.formatting._
      s"""
         |Specify the maximum number of errors to report. The default is '${ Blue(defaultValue) }'.
         |Enter '${ Blue("-1") }' to show all errors.
       """.stripMargin.trim
    }
    override val flag: String = "maxerrors"
  }

  case object IgnoreDefaultImportsFlag extends ArgumentFlag[Set[String]] {
    override val flag           = "ignoreimport"
    override val argDescription = "import"

    override def description(formatter: Formatter): String = {
      import formatter.formatting._
      s"""
         |Specify a default import to ignore.
         |Example: --${ Magenta(flag) } java::lang::object
      """.stripMargin.trim
    }
    override def value(args: Set[String]): Set[String] = args
  }

  case object MessageContextFlag extends NumberFlag {
    override val defaultValue = 2

    override val flag      = "errorcontext"
    override val shortFlag = Some("c")

    override def description(formatter: Formatter): String = {
      import formatter.formatting._
      s"""
         |Specify how many lines to display around an error position in error messages.
         |The default is '${ Blue(defaultValue) }'.
      """.stripMargin.trim
    }

  }

  case object LineWidthFlag extends NumberFlag {
    override val defaultValue: Int = -1

    val DefaultWidth = 120

    override val flag = "linewidth"

    override def description(formatter: Formatter): String = {
      import formatter.formatting._
      s"""
         |Specifies the width of a line in error messages and output.
         |If none is given (or ${ Blue(-1) } is given) the width of the terminal will be used, if it can be determined.
         |Otherwise, '${ Blue(DefaultWidth) }' will be used.
      """.stripMargin.trim
    }

    override def value(args: Set[String]): Int = {
      val givenWidth = super.value(args)
      if (givenWidth != -1)
        givenWidth
      else if (System.console() == null)
        LineWidthFlag.DefaultWidth
      else
        TerminalFactory.create().getWidth
    }

  }

  case object ColorSchemeFlag extends JsonFlag[ColorScheme] {

    override val flag          : String = "colorscheme"
    override val argDescription: String = "colormap"

    override def description(formatter: Formatter): String = {
      import formatter.formatting._
      s"""
         |Define the color scheme to use when printing error messages and code output.
         |Argument is a JSON map of colors, type --${ Magenta(HelpFlag.flag) } ${ Magenta(flag) } for more details.
      """.stripMargin.trim
    }


    override def extendedDescription(formatter: Formatter): String = {
      import formatter.formatting._
      import tlang.formatting.Colors.ColorScheme._

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
         |  \\"$VariableName\\": \\"1\\",
         |  \\"$ClassName\\":    \\"\\",
         |  \\"$MethodName\\":   \\"magenta\\",
         |  \\"$StringName\\":   \\"green\\",
         |  \\"$NumberName\\":   \\"bold\\",
         |  \\"$CommentName\\":  \\"underlined\\",
         |  \\"$SymbolName\\":    \\"black\\"
         |}
         |""".stripMargin.trim
    }
    override def value(args: Set[String]): ColorScheme = {
      if (args.isEmpty) {
        Colors.ColorScheme.DefaultColorScheme
      } else {
        val json = args.head
        JSON.parseFull(json) match {
          case Some(values: Map[_, _]) =>
            val casted = values.asInstanceOf[Map[String, String]]
            val lowercase = casted map { case (key, value) => key.toLowerCase -> value.toLowerCase }
            getColorScheme(lowercase)
          case _                       => FatalInvalidJsonArgument(Arguments.ColorSchemeFlag, json)
        }
      }
    }

    private def getColorScheme(json: Map[String, String]): ColorScheme = {
      import tlang.formatting.Colors.ColorScheme._
      json.keys
        .find { key => !(key in ColorSchemeNames) }
        .foreach { FatalInvalidColorSchemeKey(_, ColorSchemeNames) }

      val colors = ColorSchemeNames.map { name =>
        val colorValue = json.get(name) match {
          case Some(colorName) =>
            Colors.getColorValue(colorName).getOrElse(FatalInvalidColorSchemeArg(colorName, Colors.ColorNames))
          case None            => -1
        }
        name -> colorValue
      }.toMap

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
  }

  case object PhasesFlag extends BooleanFlag {
    override val flag: String = "phases"
    override def description(formatter: Formatter): String =
      "Prints information about the phases of the T-Compiler and exits."
  }

}
