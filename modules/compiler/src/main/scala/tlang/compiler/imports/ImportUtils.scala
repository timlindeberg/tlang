package tlang
package compiler
package imports

object ImportUtils {

  val seperators = "(::|/|\\.)"

  def toBCELName(name: String): String = name.replaceAll(seperators, ".")
  def toTName(name: String): String = name.replaceAll(seperators, "::")
  def toPath(name: String): String = name.replaceAll(seperators, "/")
}
