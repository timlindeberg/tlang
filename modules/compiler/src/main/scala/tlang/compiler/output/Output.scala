package tlang.compiler.output

import tlang.formatting.Formatter

trait Output {

  def pretty(formatter: Formatter): String
  def json(): Map[String, Any]

}
