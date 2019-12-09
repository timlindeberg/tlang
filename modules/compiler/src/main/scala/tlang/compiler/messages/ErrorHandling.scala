package tlang
package compiler
package messages

import tlang.formatting.ErrorStringContext

trait ErrorHandling {

  // This is a val since we need a stable identifier in order to import the string context
  val errorStringContext: ErrorStringContext

  def reporter: Reporter
  def replaceNames(str: String): String = str

  def report(warning: WarningMessage): Unit = reporter.report(warning)
  def report(fatal: FatalMessage): Nothing = {
    reporter.report(fatal)

    // Reporter will throw an exception but this is here so the type can be Nothing
    // and also if reporter is mocked
    throw new CompilationException(fatal)
  }
}
