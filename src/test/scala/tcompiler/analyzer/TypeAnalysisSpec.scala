package tcompiler.analyzer

import tcompiler.{ErrorTester, TestUtils}

class TypeAnalysisSpec extends ErrorTester {
  override def Name: String = "Type Analysis"
  override def Path: String = TestUtils.Resources + "analyzer/type"
}