package tlang
package compiler
package execution

import java.nio.file.Files

import better.files.{File, FileMonitor}
import tlang.compiler.argument.VerboseFlag
import tlang.compiler.ast.Trees.CompilationUnit
import tlang.compiler.imports.TemplateFile
import tlang.compiler.output.MessageOutput
import tlang.formatting.Formatter
import tlang.options.Options
import tlang.utils.{FileSource, Logging, Source}

import scala.collection.mutable
import scala.util.{Success, Try}

case class CompilerFileWatcher(
  ctx: Context,
  options: Options,
  sources: List[Source],
  onFilesChanged: List[Source] => Unit,
  CUs: Seq[CompilationUnit] = Nil)
  (implicit formatter: Formatter) extends Logging {

  private val watchedFiles = mutable.Set[File]()

  import formatter._

  def watch(): Unit = {

    val fileSources = sources.filterInstance[FileSource]
    if (fileSources.isEmpty) {
      ctx.output += MessageOutput(s"No file sources were given, can't use watch flag.")
      return
    }

    info"Starting file watchers"
    if (options(VerboseFlag))
      ctx.output += MessageOutput(s"Watching for ${ Green("changes") }...")
    fileSources foreach startFileWatcher

    while (true) {
      readNewFileToWatch()
    }
  }

  private def startFileWatcher(fileSource: FileSource): Unit = {
    import scala.concurrent.ExecutionContext.Implicits.global
    val rootFile = fileSource.file

    (rootFile :: getDependencies(fileSource)) foreach { file =>
      if (!watchedFiles.contains(file)) {
        watchedFiles += file
        val monitor = CompilerFileMonitor(file, rootFile)
        info"Watching file ${ monitor.fileToWatch } for changes to ${ monitor.fileToCompile }"
        monitor.start()
      }
    }
  }

  private def getDependencies(fileSource: FileSource): List[File] = {
    val cu = CUs.find(_.source.get == fileSource)
    if (cu.isEmpty)
      return Nil

    cu.get.imports.entries
      .flatMap { case (_, fullName) =>
        ctx.classPath(fullName).
          filter { _.isInstanceOf[TemplateFile] }
          .map { f => File(f.path) }
      }
      .toList
  }

  private def readNewFileToWatch(): Unit = {
    val fileName = scala.io.StdIn.readLine()

    info"Got new file to watch: $fileName"

    val file = Try(File(fileName)) match {
      case Success(file) if file.exists => file
      case _                            =>
        ctx.output += MessageOutput(s"No such file: ${ Red(fileName) }")
        return
    }
    if (watchedFiles.contains(file)) {
      ctx.output += MessageOutput(s"File ${ Red(fileName) } is already being watched.")
      return
    }

    ctx.output += MessageOutput(s"Watching new file: ${ Green(fileName) }")

    val fileSource = FileSource(file)

    onFilesChanged(fileSource :: Nil)
    startFileWatcher(fileSource)
  }

  case class CompilerFileMonitor(fileToWatch: File, fileToCompile: File) extends FileMonitor(fileToWatch, fileToWatch.isDirectory) {

    private val filesSource = FileSource(fileToCompile)
    private val modifiedTimes = mutable.Map[String, Long]()

    override def onModify(file: File, count: Int): Unit = {
      import ctx.formatter._

      if (hasAlreadyHandled(file))
        return

      info"$file changed"
      if (options(VerboseFlag))
        ctx.output += MessageOutput(s"Found changes to file ${ Magenta(file.path.relativePWD) }.")

      FileSource.clearCache()
      ctx.reporter.clear()

      onFilesChanged(filesSource :: Nil)
    }

    // On Windows the modified event is sometimes triggered twice
    private def hasAlreadyHandled(file: File): Boolean = {
      val lastModified = Files.getLastModifiedTime(file.path).toMillis
      val path = file.pathAsString
      if (modifiedTimes.get(path).contains(lastModified))
        return true

      modifiedTimes(path) = lastModified
      false
    }
  }

}
