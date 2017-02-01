package tcompiler

import java.io.File
import java.nio.file.{InvalidPathException, Paths}

import tcompiler.Flags.{Flag, _}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer


class FlagArgs extends mutable.HashMap[Flag, mutable.Set[String]] with mutable.MultiMap[Flag, String] {
  override def apply(key: Flag): mutable.Set[String] = getOrElse(key, mutable.Set())
}

/**
  * Created by Tim Lindeberg on 2/1/2017.
  */
class Options(arguments: Array[String]) extends MainErrors {

  val flagArgs : FlagArgs           = new FlagArgs()
  val filePaths: ListBuffer[String] = new ListBuffer()

  def apply(flag: BooleanFlag): Boolean = flagArgs(flag).nonEmpty
  def apply(flag: ArgumentFlag): Set[String] = flagArgs(flag).toSet // immutable

  private def processOptions(args: List[String]): Unit = {
    if (args eq Nil)
      return

    val rest = args match {
      case OptionalArgumentFlag(flag) :: Nil              =>
        flagArgs.addBinding(flag, flag.defaultArg)
        Nil
      case OptionalArgumentFlag(flag) :: maybeArg :: rest =>
        val args = splitArgs(maybeArg).filter(flag.isValidArg)
        if (args.nonEmpty) {
          args foreach { arg => flagArgs.addBinding(flag, arg.toLowerCase) }
          rest
        } else {
          flagArgs.addBinding(flag, flag.defaultArg)
          maybeArg :: rest
        }
      case ArgumentFlag(flag) :: arg :: rest              =>
        splitArgs(arg) foreach { arg => flagArgs.addBinding(flag, arg) }
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

  processOptions(arguments.toList)

  val maxErrors     : Int          = getNum(MaxErrors, MaxErrors.Default)
  val errorContext  : Int          = getNum(ErrorContext, ErrorContext.Default)
  val classPaths    : List[String] = getClassPaths(flagArgs(ClassPath))
  val files         : List[File]   = getFilesToCompile(filePaths)
  val outDirectories: List[File]   = getOutDirectories(flagArgs(Directory))

  private def splitArgs(arg: String): List[String] = arg.split(",").map(_.trim).filter(_.nonEmpty).toList

  private def getNum(flag: Flag, defaultValue: Int): Int = {
    val validNums = flagArgs(flag).map { num =>
      try {
        num.toInt
      } catch {
        case _: NumberFormatException => FatalInvalidNumber(flag, num)
      }
    }
    if (validNums.isEmpty) defaultValue else validNums.max
  }

  private def getClassPaths(paths: mutable.Set[String]): List[String] = {
    val p = paths.toList
    p.filter(!isValidPath(_)).foreach(FatalInvalidClassPath)
    Main.TDirectory :: p
  }

  private def getOutDirectories(paths: mutable.Set[String]): List[File] = {
    if (paths.isEmpty)
      return List(new File("."))

    paths.map { path =>
      if (!isValidPath(path))
        FatalInvalidOutputDirectory(path)
      new File(path)
    }.toList
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

  private def getFilesToCompile(paths: ListBuffer[String]): List[File] = {
    val files = paths.flatMap { path =>
      if (path.startsWith("-"))
        FatalInvalidFlag(path)

      val file = new File(path)
      if (file.isDirectory) {
        val tFiles = file.listFiles().filter(_.getName.endsWith(Main.FileEnding))
        if (tFiles.isEmpty)
          FatalGivenDirectoryContainsNoTFiles(path)

        tFiles.toList
      } else {
        if (!file.getName.endsWith(Main.FileEnding))
          FatalGivenFileIsNotTFile(path)

        List(file)
      }
    }

    files.filter(!_.exists()).foreach(f => FatalCannotFindFile(f.getPath))
    files.toList
  }
}
