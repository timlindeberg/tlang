package tlang
package compiler
package output

import tlang.utils.JSON.Json

trait Output {

  def pretty: String
  def json: Json
}
