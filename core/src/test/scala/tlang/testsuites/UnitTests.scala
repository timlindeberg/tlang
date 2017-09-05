package tlang.testsuites

import org.scalatest.Suites
import tlang.testutils.{PackageSuite, UnitSpec}

class UnitTests extends Suites(
  Compiler,
  Repl,
  Formatting,
  Messages,
  Options,
  Utils
)

// If these are objects they wont get picked up by the test runner
object Compiler extends PackageSuite[UnitSpec]("tlang.compiler")
object Repl extends PackageSuite[UnitSpec]("tlang.repl")
object Formatting extends PackageSuite[UnitSpec]("tlang.formatting")
object Messages extends PackageSuite[UnitSpec]("tlang.messages")
object Options extends PackageSuite[UnitSpec]("tlang.options")
object Utils extends PackageSuite[UnitSpec]("tlang.utils")
