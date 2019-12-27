package tlang
package compiler

import better.files.File
import org.scalatest.ParallelTestExecution
import tlang.compiler.analyzer.{Flowing, Naming, Typing}
import tlang.compiler.ast.Parsing
import tlang.compiler.lexer.Lexing
import tlang.compiler.modification.Templating
import tlang.testutils.TestConstants._
import tlang.utils.Source

class CompilerErrorsSuite extends CompilerIntegrationTestSpec with ParallelTestExecution {

  val ErrorResources = s"$Resources/errortests"
  val MessageContextSize = 3

  override def suiteName: String = "Compiler Errors"

  testFileForErrors(s"$ErrorResources/Lexing", Lexing)
  testFileForErrors(s"$ErrorResources/Parsing", Lexing andThen Parsing)
  testFileForErrors(s"$ErrorResources/Imports", Lexing andThen Parsing)
  testFileForErrors(s"$ErrorResources/Templating", Lexing andThen Parsing andThen Templating)
  testFileForErrors(s"$ErrorResources/Naming", Lexing andThen Parsing andThen Templating andThen Naming)
  testFileForErrors(s"$ErrorResources/Typing", Lexing andThen Parsing andThen Templating andThen Naming andThen Typing)
  testFileForErrors(s"$ErrorResources/Flowing", Lexing andThen Parsing andThen Templating andThen Naming andThen Typing andThen Flowing)

  private def testFileForErrors[T](path: String, pipeline: CompilerPhase[Source, T]): Unit = testFiles(path, testFileForErrors(pipeline, _))

  private def testFileForErrors[T](pipeline: CompilerPhase[Source, T], file: File): Unit = {
    testFile(pipeline, file)
  }
}
