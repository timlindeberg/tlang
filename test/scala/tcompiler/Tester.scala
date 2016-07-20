package tcompiler

import java.io.File

import org.scalatest.{BeforeAndAfter, FlatSpec, Matchers}
import tcompiler.ast.Trees.CompilationUnit
import tcompiler.imports.ClassSymbolLocator
import tcompiler.utils.Pipeline

/**
  * Created by Tim Lindeberg on 4/11/2016.
  */
trait Tester extends FlatSpec with Matchers with BeforeAndAfter {

  import TestUtils._

  val PrintErrors = true

  def Name: String

  def Path: String

  def Pipeline: Pipeline[List[File], List[CompilationUnit]]

  behavior of Name
  TestUtils.programFiles(Path).foreach(test)

  before {
    ClassSymbolLocator.clearCache()
  }

  def test(file: File): Unit = {
    if (file.isDirectory)
      programFiles(file.getPath).foreach(test)
    else if (shouldBeIgnored(file))
      ignore should file.getName in testFile(file)
    else
      it should file.getName in testFile(file)
  }


  def testFile(file: File): Unit

}
