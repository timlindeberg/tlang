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
}
