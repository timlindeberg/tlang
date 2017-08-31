package tlang.options.arguments

import tlang.compiler.imports.Imports
import tlang.formatting.Formatter
import tlang.messages.ErrorStringContext
import tlang.options.ArgumentFlag
import tlang.utils.Extensions._

case object IgnoreDefaultImportsFlag extends ArgumentFlag[Set[String]] {
  override val name           = "ignoreimport"
  override val argDescription = "import"

  override protected def verifyArgument(ignoredImport: String)(implicit errorContext: ErrorStringContext): Unit = {
    import errorContext.ErrorStringContext

    if (ignoredImport.toLowerCase notIn Imports.DefaultImportNames.map(_.toLowerCase)) {
      val suggestion = errorContext.suggestion(ignoredImport, Imports.DefaultImportNames)
      error(err"There is no default import called $ignoredImport.$suggestion")
    }
  }

  override def description(formatter: Formatter): String = {
    import formatter.formatting._
    s"""
       |Specify a default import to ignore.
       |Example: --${ Magenta(name) } java::lang::object
      """.stripMargin.trim
  }

  override def parseValue(ignoredImports: Set[String]): Set[String] = {
    ignoredImports.map { ignoredImport =>
      Imports.DefaultImportNames.find { _.toLowerCase == ignoredImport.toLowerCase }.get
    }
  }
}