package tlang
package compiler
package execution

case class ExitException(code: Int, forceExit: Boolean = false) extends Throwable
