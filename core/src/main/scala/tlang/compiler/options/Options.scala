package tlang.compiler.options

import java.io.File
import java.nio.file.{InvalidPathException, Paths}

import tlang.Constants
import tlang.compiler.options.Flags._
import tlang.compiler.{Main, MainErrors}
import tlang.formatting.BoxStyles.{Ascii, BoxStyle}
import tlang.formatting.Colors.ColorScheme
import tlang.formatting._
import tlang.utils.Extensions._

import scala.collection.mutable
import scala.tools.jline.TerminalFactory
import scala.util.parsing.json.JSON


class FlagArgs extends mutable.HashMap[Flag, mutable.Set[String]] with mutable.MultiMap[Flag, String] {
  override def apply(key: Flag): mutable.Set[String] = getOrElse(key, mutable.Set())
}

case class Options(arguments: Array[String]) extends MainErrors {

  private val flagArgs : FlagArgs            = new FlagArgs()
  private val filePaths: mutable.Set[String] = mutable.Set()

  def apply(flag: BooleanFlag): Boolean = flagArgs(flag).nonEmpty
  def apply(flag: ArgumentFlag): Set[String] = flagArgs(flag).toSet
  def apply(flag: NumberFlag): Int = getNum(flag)

  private def processOptions(args: List[String]): Unit = {
    if (args eq Nil)
      return

    val rest = args match {
      case JsonFlag(flag) :: rest                         =>
        val s = rest.mkString(" ")
        val jsonStart = s.indexOf("{")
        val jsonEnd = s.lastIndexOf("}")
        if (jsonStart == -1 || jsonEnd == -1)
          FatalInvalidJsonArgument(flag, s)

        val json = s.substring(jsonStart, jsonEnd + 1)
        flagArgs.addBinding(flag, json)
        val afterJson = s.substring(jsonEnd + 1).trim
        afterJson.split(" ").toList
      case OptionalArgumentFlag(flag) :: Nil              =>
        flagArgs.addBinding(flag, flag.defaultArg)
        Nil
      case OptionalArgumentFlag(flag) :: maybeArg :: rest =>
        val allArgs = splitArgs(maybeArg)
        if (allArgs.exists(flag.isValidArg)) {
          allArgs foreach { arg => flagArgs.addBinding(flag, arg.toLowerCase) }
          rest
        } else {
          flagArgs.addBinding(flag, flag.defaultArg)
          maybeArg :: rest
        }
      case ArgumentFlag(flag) :: arg :: rest              =>
        addArgs(flag, arg)
        rest
      case BooleanFlag(flag) :: rest                      =>
        flagArgs.addBinding(flag, "Active")
        rest
      case filePath :: rest                               =>
        filePaths += filePath
        rest
    }
    processOptions(rest)
  }

  private def addArgs(flag: Flag, arg: String) =
    splitArgs(arg) foreach { arg => flagArgs.addBinding(flag, arg.toLowerCase) }

  processOptions(arguments.toList)

  verifyOutputPhases(flagArgs(PrintOutput))


  val classPaths: Set[String] = {
    val paths = flagArgs(ClassPathFlag)
    paths.filter(!isValidPath(_)).foreach(FatalInvalidClassPath)
    paths.toSet
  }

  val outDirectories: Set[File] = {
    val paths = flagArgs(Directory)
    if (paths.isEmpty)
      Set(new File("."))
    else {
      paths.map { path =>
        if (!isValidPath(path))
          FatalInvalidOutputDirectory(path)
        new File(path)
      }.toSet
    }
  }

  val files: Set[File] = {
    val files = filePaths.flatMap { path =>
      if (path.startsWith("-"))
        FatalInvalidFlag(path, Flag.flagNames)

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
    files.toSet
  }

  val boxStyle: BoxStyle = {
    val formattings = flagArgs(Flags.FormattingStyle)
    val boxNames = BoxStyles.All.map(_.styleName.toLowerCase)
    formattings.foreach { formatting =>
      if (!(formatting in boxNames))
        FatalInvalidArgToFlag(Flags.FormattingStyle, formatting, boxNames)
    }
    formattings.headOption
      .flatMap(formatting => BoxStyles.All.find(_.styleName.toLowerCase == formatting))
      .getOrElse(BoxStyles.DefaultBox)
  }

  val colorScheme: ColorScheme = {
    val jsons = flagArgs(Flags.ColorScheme)
    if (jsons.isEmpty) {
      ColorScheme.DefaultColorScheme
    } else {
      val json = jsons.head
      JSON.parseFull(jsons.head) match {
        case Some(values: Map[_, _]) =>
          val casted = values.asInstanceOf[Map[String, String]]
          val lowercase = casted map { case (key, value) => key.toLowerCase -> value.toLowerCase }
          getColorScheme(lowercase)
        case _                       => FatalInvalidJsonArgument(Flags.ColorScheme, json)
      }
    }
  }

  val lineWidth: Int = {
    val givenWidth = apply(LineWidth)
    if (givenWidth != -1)
      givenWidth
    else if (System.console() == null)
      LineWidth.DefaultWidth
    else
      TerminalFactory.create().getWidth
  }

  val formatting: Formatting = Formatting(
    boxStyle, lineWidth, colorScheme,
    useColor = !apply(Flags.NoColor),
    asciiOnly = boxStyle == Ascii
  )

  private def verifyOutputPhases(phases: mutable.Set[String]): Unit = {
    val validPhases = Main.CompilerPhases.map(_.phaseName)
    phases.foreach { phase =>
      if (!(phase in validPhases))
        FatalInvalidArgToFlag(PrintOutput, phase, validPhases)
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

  private def splitArgs(arg: String): List[String] = arg.split(",").map(_.trim).filter(_.nonEmpty).toList

  private def getNum(flag: NumberFlag): Int = {
    val validNums = flagArgs(flag).map { num =>
      try {
        num.toInt
      } catch {
        case _: NumberFormatException => FatalInvalidArgToFlag(flag, num, Nil)
      }
    }
    if (validNums.isEmpty) flag.defaultValue else validNums.max
  }

  private def isValidPath(path: String): Boolean = {
    try {
      Paths.get(path)
    } catch {
      case _: InvalidPathException =>
        return false
    }
    !new File(path).isFile
  }


}
