package tcompiler.code

import tcompiler.{TestUtils, ValidTester}
import tcompiler.analyzer.{NameAnalysis, TypeChecking}
import tcompiler.ast._
import tcompiler.lexer.Lexer
import tcompiler.modification.{Imports, Templates}


class STDSpec extends ValidTester {

  override def Name: String = "Standard Lib"
  override def Path: String = TestUtils.Resources + "stdtests"

}
