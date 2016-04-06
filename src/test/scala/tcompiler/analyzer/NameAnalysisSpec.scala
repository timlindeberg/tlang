package tcompiler.analyzer

import tcompiler.{ErrorTester, TestUtils}

class NameAnalysisSpec extends ErrorTester {
  override def Name: String = "Name Analysis"
  override def Path: String = TestUtils.Resources + "analyzer/name"
}