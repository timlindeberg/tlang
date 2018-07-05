package tlang.compiler.output

import tlang.formatting.Formatter
import tlang.utils.JSON.Json

trait Output {

  def pretty(formatter: Formatter): String
  def json: Json

}
