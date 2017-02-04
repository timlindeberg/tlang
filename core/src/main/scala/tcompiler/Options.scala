package tcompiler

import java.io.File
import java.nio.file.{InvalidPathException, Paths}

import tcompiler.Flags.{Flag, _}
import tcompiler.error.Boxes
import tcompiler.error.Boxes.Box
import tcompiler.utils.Extensions._

import scala.collection.mutable


class FlagArgs extends mutable.HashMap[Flag, mutable.Set[String]] with mutable.MultiMap[Flag, String] {
  override def apply(key: Flag): mutable.Set[String] = getOrElse(key, mutable.Set())
}

/**
  * Created by Tim Lindeberg on 2/1/2017.
  */
case class Options(arguments: Array[String]) extends MainErrors {

  val flagArgs : FlagArgs            = new FlagArgs()
  val filePaths: mutable.Set[String] = mutable.Set()

  def apply(flag: BooleanFlag): Boolean = flagArgs(flag).nonEmpty
  def apply(flag: ArgumentFlag): Set[String] = flagArgs(flag).toSet
  def apply(flag: NumberFlag): Int = getNum(flag)

  private def processOptions(args: List[String]): Unit = {
    if (args eq Nil)
      return

    val rest = args match {
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
    splitArgs(arg) foreach { arg => flagArgs.addBinding(flag, arg) }

  processOptions(arguments.toList)

  verifyOutputStages(flagArgs(PrintOutput))


  val classPaths: Set[String] = {
    val paths = flagArgs(ClassPath)
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
    files.toSet
  }

  val boxType: Box = {
    val formattings = flagArgs(Formatting)
    val boxNames = Boxes.All.map(_.name)
    formattings.foreach { formatting =>
      if (!(formatting in boxNames))
        FatalInvalidArgToFlag(Formatting, formatting, boxNames.toList)
    }
    formattings.headOption
      .flatMap(formatting => Boxes.All.find(_.name == formatting))
      .getOrElse(Boxes.DefaultBox)
  }


  private def verifyOutputStages(stages: mutable.Set[String]): Unit = {
    val validStages = Main.CompilerStages.map(_.stageName)
    stages.foreach { stage =>
      if (!(stage in validStages))
        FatalInvalidArgToFlag(PrintOutput, stage, validStages)
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
