package tlang
package utils

import tlang.formatting.grid.Grid
import tlang.formatting.textformatters.SyntaxHighlighter
import tlang.formatting.{ColorSpec, Formatter}
import tlang.testutils.UnitSpec

class ClassLocatorSpec extends UnitSpec {

  it should "locate classes in a package" in {
    val classesInFormattingPackage = ClassLocator.getClassesInPackage("tlang.formatting")
    classesInFormattingPackage should contain(classOf[Grid])
    classesInFormattingPackage should contain(classOf[ColorSpec])
    classesInFormattingPackage should contain(classOf[SyntaxHighlighter])
    classesInFormattingPackage should contain(classOf[Formatter])
  }
}
