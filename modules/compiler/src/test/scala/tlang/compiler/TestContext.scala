package tlang.compiler

import better.files.File
import tlang.Constants
import tlang.testutils.TestConstants
import tlang.compiler.imports.ClassPath
import tlang.compiler.messages.DefaultReporter
import tlang.compiler.output.{JSONOutputHandler, PrettyOutputHandler}
import tlang.compiler.utils.TLangSyntaxHighlighter
import tlang.formatting.textformatters.SyntaxHighlighter
import tlang.testutils.TestConstants.{PrintCodePhases, PrintJSON, Resources, TestOutputDirectory}

trait TestContext {

  import TestConstants.TestFormatter

  val TestContext: Context = testContext(None)
  implicit val SyntaxHighlighter: SyntaxHighlighter = TLangSyntaxHighlighter()

  def testContext(file: Option[File]): Context = {
    val outDir = file match {
      case Some(f) =>
        val resourceDir = File(Resources)
        val mainName = f.pathAsString.stripPrefix(resourceDir.pathAsString).stripSuffix(Constants.FileEnding)
        File(s"$TestOutputDirectory/$mainName/")
      case None    => File(".")
    }

    val outputHandler = if(PrintJSON) JSONOutputHandler() else PrettyOutputHandler()
    Context(
      reporter = DefaultReporter(),
      output = outputHandler,
      outDirs = Set(outDir),
      classPath = ClassPath.Default,
      printCodePhase = PrintCodePhases
    )
  }

}
