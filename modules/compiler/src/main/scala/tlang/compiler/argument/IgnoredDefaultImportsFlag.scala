package tlang
package compiler
package argument

import tlang.compiler.imports.Imports
import tlang.formatting.{ErrorStringContext, Formatter}
import tlang.options.ArgumentFlag


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

  override def description(implicit formatter: Formatter): String =
    s"""
       |Specify a default import to ignore.
       |Example: ${flag(this)} ${highlight("java::lang::Object")}
      """

  override def parseValue(ignoredImports: Set[String]): Set[String] = {
    ignoredImports.map { ignoredImport =>
      Imports.DefaultImportNames.find { _.toLowerCase == ignoredImport.toLowerCase }.get
    }
  }
}
