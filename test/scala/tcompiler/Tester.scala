package tcompiler

import java.io.File

import org.scalatest.{FlatSpec, Matchers}
import tcompiler.ast.Trees.Program
import tcompiler.utils.Pipeline

/**
  * Created by Tim Lindeberg on 4/11/2016.
  */
trait Tester extends FlatSpec with Matchers {

  import TestUtils._

  val PrintErrors = false

  def Name: String
  def Path: String
  def Pipeline: Pipeline[List[File], List[Program]]

  behavior of Name
  TestUtils.programFiles(Path).foreach(test)

  def test(file: File): Unit =
    if (file.isDirectory){
      programFiles(file.getPath).foreach(testFile)
    } else{
      if(shouldBeIgnored(file))
        ignore should file.getName in testFile(file)
      else
        it should file.getName in testFile(file)
    }

  def testFile(file: File): Unit

}
