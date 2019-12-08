package tlang
package testutils

import org.scalatest.Tag
import tlang.formatting.Formatter

import scala.util.Try
import scala.util.matching.Regex

object TestConstants {

  val Resources: String = "src/test/resources"
  val TestOutputDirectory: String = "integrationTestFiles"

  val AsciiOnly: Boolean = sys.env.get("ascii").contains("true")
  val UseColors: Boolean = sys.env.get("useColors").contains("true")
  val PrintErrors: Boolean = sys.env.get("printErrors").contains("true")
  val KeepFilesOnExit: Boolean = sys.env.get("keepFilesOnExit").contains("true")
  val Verbose: Boolean = sys.env.get("verbose").contains("true")
  val PrintJSON: Boolean = sys.env.get("printJson").contains("true")
  val LineWidth: Int = sys.env.get("lineWidth").flatMap(num => Try(num.toInt).toOption).getOrElse(80)
  val PrintCodePhases: Set[String] = sys.env.get("printOutput").map(_.split(", *").map(_.trim).toSet).getOrElse(Set())

  implicit val TestFormatter: Formatter = Formatter(LineWidth, useColor = UseColors, asciiOnly = AsciiOnly)

  val SolutionRegex: Regex = """.*// *[R|r]es:(.*)""".r
  val IgnoreRegex: Regex = """// *[I|i]gnore""".r

  object CompilerIntegrationTestTag extends Tag("compilerIntegrationTest")

}
