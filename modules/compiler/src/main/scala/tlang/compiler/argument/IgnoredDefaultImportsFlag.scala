package tlang.compiler.argument

import tlang.compiler.imports.Imports
import tlang.formatting.{ErrorStringContext, Formatter}
import tlang.options.ArgumentFlag
import tlang.utils.Extensions._

case object IgnoredDefaultImportsFlag extends ArgumentFlag[Set[String]] {
  override val name           = "ignoreimport"
  override val argDescription = "import"

  override protected def verify(ignoredImport: String)(implicit errorContext: ErrorStringContext): Unit = {
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