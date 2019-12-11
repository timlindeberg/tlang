package tlang
package compiler
package execution

case class ExitException(code: Int) extends Throwable
