package tcompiler

import tcompiler.utils.EnumerationMacros

/**
  * Created by Tim Lindeberg on 5/13/2016.
  */
object Flags {

  sealed abstract class Flag extends Ordered[Flag] with Product with Serializable {

    implicit def flag2String(f: Flag): String = f.flag

    val flag       : String
    val description: String
    val arg: String = ""
    def format: String = {
      val lines = description.split("\n")
      val format = f"  ${flag + " " + arg}%-20s${lines.head}\n"

      val space = " "
      lines.tail.foldLeft(format)((format, s) => format + f"  $space%-20s$s%-50s\n")
    }
    def compare(that: Flag) = flag.length - that.flag.length
  }

  case object Exec extends Flag {
    override val flag        = "-exec"
    override val description = "Executes the program after compilation."
  }

  case object SuppressWarnings extends Flag {
    override val flag        = "-nowarn"
    override val description = "Suppresses warning messages."
  }

  case object PrintGeneratedCode extends Flag {
    override val flag        = "-printcode"
    override val description = "Pretty prints the AST as it looks before code is generated."
  }

  case object Help extends Flag {
    override val flag        = "-help"
    override val description = "Prints help information and exits."
  }

  case object Directory extends Flag {
    override val flag        = "-d"
    override val description = "Specify the path where generated classes are placed."
    override val arg         = "<directory>"
  }

  case object Version extends Flag {
    override val flag        = "-version"
    override val description = "Prints version information and exits."
  }

  case object WarningIsError extends Flag {
    override val flag        = "-Werror"
    override val description = "Treats warnings as errors and exits compilation."
  }

  case object NoColor extends Flag {
    override val flag        = "-nocolor"
    override val description = "Prints error messages without ANSI-coloring."
  }

  case object ClassPath extends Flag {
    override val flag        = "-cp"
    override val description = "Specify a path where classes should be searched for."
    override val arg         = "<directory>"
  }

  case object MaxErrors extends Flag {
    val DefaultMax = 100

    override val flag        = "-maxerrors"
    override val description = s"Specify the maximum number of errors to report. The default is $DefaultMax.\nEnter -1 to show all errors."
    override val arg         = "<num>"
  }

}
