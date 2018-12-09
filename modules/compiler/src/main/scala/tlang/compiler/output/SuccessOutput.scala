package tlang
package compiler
package output

import tlang.utils.JSON.Json

case class SuccessOutput(success: Boolean) extends Output {
  override def pretty: String = ""
  override def json: Json = Json("success" -> success)
}
