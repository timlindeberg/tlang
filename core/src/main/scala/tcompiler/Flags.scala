package tcompiler

import tcompiler.code.Desugaring
import tcompiler.error.Boxes
import tcompiler.utils.Extensions._
import tcompiler.utils.{Colors, Enumeration}

/**
  * Created by Tim Lindeberg on 5/13/2016.
  */
object Flags {

  sealed abstract class Flag extends Ordered[Flag] with Product with Serializable {

    val flag: String
    val shortFlag: Option[String] = None
    val description: String

    def format(colors: Colors): String = {
      import colors._

      val lines = description.stripMargin.trim.split("\n")
      val firstRow = f"  $flagDescription%-26s${lines.head}\n"

      val space = " " * 28
      val fullDescription = lines.tail.foldLeft(firstRow)((description, s) => description + s"$space$s\n")
      if (!colors.active)
        return fullDescription

      val colorization: List[String => String] = List(
        """\<([a-z]*)\>""".r.replaceAllIn(_, m => s"<${Blue(m.group(1))}>"),
        """--([a-z]*)""".r.replaceAllIn(_, m => s"--${Magenta(m.group(1))}"),
        """\(-([a-z]*)\)""".r.replaceAllIn(_, m => s"(-${Magenta(m.group(1))})")
      )


      colorization.foldLeft(fullDescription)((s, addColor) => addColor(s))
    }

    def flagDescription: String = {
      val shortFlagDescription = shortFlag.map(f => s" (-$f)").getOrElse("")
      s"--$flag$shortFlagDescription "
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


    override def flagDescription: String = {
      // Dropping space
      super.flagDescription.dropRight(1) + s" <$arg> "
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

  //--------------------------------------------------------------------------------
  //-- Flags
  //--------------------------------------------------------------------------------

  case object Exec extends BooleanFlag {
    override val flag = "exec"

    override val description =
      """
        |Executes the program after compilation.
      """
  }

  case object SuppressWarnings extends BooleanFlag {
    override val flag = "nowarn"

    override val description =
      """
        |Suppresses warning messages.
      """
  }

  case object PrintOutput extends OptionalArgumentFlag {
    override val flag       = "printoutput"
    override val arg        = "stage"
    override val defaultArg = Desugaring.stageName

    override def isValidArg(arg: String): Boolean = arg.toLowerCase in validArgs


    private val validArgs = Main.CompilerStages.map(_.stageName)

    override val description =
      """
        |Prints the output after a given compiler stage. This can be the tokens
        |produced by the Lexer, a pretty printed AST or the byte code generated
        |by the CodeGeneration stage.
        |If no argument is given the code is printed as it looks before the
        |final code is generated. Type --help stages to list compiler stages.
      """
  }

  case object Verbose extends BooleanFlag {
    override val flag      = "verbose"
    override val shortFlag = Some("v")

    override val description =
      """
        |Prints additional information during compilation such as elapsed time
        |for each compilation stage.
      """
  }

  case object Help extends OptionalArgumentFlag {
    override val flag       = "help"
    override val shortFlag  = Some("h")
    override val arg        = "about"
    override val defaultArg = ""

    override def isValidArg(arg: String): Boolean = arg.toLowerCase in validArgs

    private val validArgs = List("stages")


    override val description =
      """
        |Prints help information and exits. Giving 'stages' as an argument
        |lists the different compiler stages.
      """
  }

  case object Directory extends ArgumentFlag {
    override val flag      = "directory"
    override val shortFlag = Some("d")
    override val arg       = "dir"

    override val description =
      """
        |Specify the path where generated classes are placed.
      """
  }

  case object Version extends BooleanFlag {
    override val flag = "version"

    override val description =
      """
        |Prints version information and exits.
      """
  }

  case object WarningIsError extends BooleanFlag {
    override val flag = "werror"

    override val description =
      """
        |Treats warnings as errors and exits compilation.
      """
  }

  case object Formatting extends ArgumentFlag {
    val Default = "Simple"

    override val flag = "formatting"
    override val arg  = "style"

    override val description =
      s"""
         |Chooses the formatting style of messages produced by the compiler.
         |'Simple' will only produce ASCII-characters and use no colors.
         |Valid styles are: ${Boxes.All.map(_.name).mkString(", ")}
      """
  }

  case object ClassPath extends ArgumentFlag {
    override val flag      = "classpath"
    override val shortFlag = Some("cp")
    override val arg       = "dir"

    override val description =
      """
        |Specify a path where classes should be searched for.
      """
  }

  case object MaxErrors extends NumberFlag {
    override val defaultValue = 100
    override val flag         = "maxerrors"

    override val description =
      s"""
         |Specify the maximum number of errors to report. The default is $defaultValue.
         |Enter -1 to show all errors.
       """
  }

  case object IgnoreDefaultImports extends ArgumentFlag {
    override val flag = "ignoreimport"
    override val arg  = "import"

    override val description =
      """
        |Specify a default import to ignore.
        |E.g. ignoreimport java::lang::object
      """
  }

  case object ErrorContext extends NumberFlag {
    override val defaultValue = 2

    override val flag      = "errorcontext"
    override val shortFlag = Some("c")
    override val arg       = "num"


    override val description =
      s"""
         |Specify how many lines to display around an error position in error messages.
         |The default is $defaultValue.
      """
  }

  case object LineWidth extends NumberFlag {
    override val defaultValue = 80

    override val flag = "linewidth"
    override val arg  = "num"


    override val description =
      s"""
         |Specify the width of a line in error message and output.
         |Default is $defaultValue chars.
      """
  }

  // These have to be defined below the Flags, otherwise the macro won't work

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

  object Flag {
    lazy val All: List[Flag] = (OptionalArgumentFlag.All ++ BooleanFlag.All ++ ArgumentFlag.All).sortBy(_.flag)
  }

}
