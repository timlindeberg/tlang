package tcompiler

import tcompiler.utils.{Colored, Enumeration}

/**
  * Created by Tim Lindeberg on 5/13/2016.
  */
object Flags {


  sealed abstract class Flag extends Ordered[Flag] with Product with Serializable with Colored {

    val flag: String
    val shortFlag: Option[String] = None
    val arg      : Option[String] = None
    val description: String

    var useColor = false

    val Colorization: List[String => String] = List(
      """\<([a-z]*)\>""".r.replaceAllIn(_, m => s"<${Blue(m.group(1))}>"),
      """--([a-z]*)""".r.replaceAllIn(_, m => s"--${Magenta(m.group(1))}"),
      """\(-([a-z]*)\)""".r.replaceAllIn(_, m => s"(-${Magenta(m.group(1))})")
    )

    def format(useColor: Boolean): String = {
      this.useColor = useColor

      val shortFlagDescription = shortFlag.map(f => s" (-$f)").getOrElse("")
      val argDescription = arg.map(a => s" <$a>").getOrElse("")
      val flagDescription = s"--$flag$shortFlagDescription$argDescription "

      val lines = description.stripMargin.trim.split("\n")
      val firstRow = f"  $flagDescription%-25s${lines.head}\n"

      val space = " " * 27
      val fullDescription = lines.tail.foldLeft(firstRow)((description, s) => description + s"$space$s\n")
      if (!useColor)
        return fullDescription

      Colorization.foldLeft(fullDescription)((s, addColor) => addColor(s))
    }

    def compare(that: Flag): Int = flag.length - that.flag.length
    def unapply(str: String): Boolean = {
      val lower = str.toLowerCase
      lower == s"--$flag" || shortFlag.contains(s"-$lower")
    }

  }

  case object Exec extends Flag {
    override val flag = "exec"

    override val description =
      """
        |Executes the program after compilation.
      """
  }

  case object SuppressWarnings extends Flag {
    override val flag = "nowarn"

    override val description =
      """
        |Suppresses warning messages.
      """
  }

  case object PrintCode extends Flag {
    override val flag = "printcode"
    override val arg  = Some("stage")

    override val description =
      """
        |Pretty prints the AST as it after the given compiler stage.
        |If no argument is given the code is printed as it looks before the
        |final code is generated. Type -help stages to list compiler stages.
      """
  }

  case object PrintInfo extends Flag {
    override val flag = "printinfo"

    override val description =
      """
        |Prints additional information during compilation such as elapsed
        |time for each compilation stage.
      """
  }

  case object Help extends Flag {
    override val flag      = "help"
    override val shortFlag = Some("h")
    override val arg       = Some("about")

    override val description =
      """
        |Prints help information and exits. Giving stages as an argument
        |lists the different compiler stages.
      """
  }

  case object Directory extends Flag {
    override val flag      = "directory"
    override val shortFlag = Some("d")
    override val arg       = Some("dir")

    override val description =
      """
        |Specify the path where generated classes are placed.
      """
  }

  case object Version extends Flag {
    override val flag      = "version"
    override val shortFlag = Some("v")

    override val description =
      """
        |Prints version information and exits.
      """
  }

  case object WarningIsError extends Flag {
    override val flag = "werror"

    override val description =
      """
        |Treats warnings as errors and exits compilation.
      """
  }

  case object NoColor extends Flag {
    override val flag = "nocolor"

    override val description =
      """
        |Prints error messages and generated code without ANSI-coloring.
      """
  }

  case object ClassPath extends Flag {
    override val flag      = "classpath"
    override val shortFlag = Some("cp")
    override val arg       = Some("dir")

    override val description =
      """
        |Specify a path where classes should be searched for.
      """
  }

  case object MaxErrors extends Flag {
    val DefaultMax = "100"

    override val flag = "maxerrors"
    override val arg  = Some("num")

    override val description =
      s"""
         |Specify the maximum number of errors to report.The default is $DefaultMax.
         |Enter -1 to show all errors.
       """
  }

  case object IgnoreDefaultImports extends Flag {
    override val flag = "ignoreimport"
    override val arg  = Some("import")

    override val description =
      """
        |Specify a default import to ignore.
        |E.g. ignoreimport java::lang::object
      """
  }

  object Flag {

    val AllFlags: List[Flag] = Enumeration.instancesOf[Flag].toList.sortBy(_.flag)
    def unapply(str: String): Option[Flag] = AllFlags.find(_.unapply(str))

  }

}
