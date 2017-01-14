package tcompiler.code

import java.io.{File, FileWriter, IOException}

import org.scalatest.{BeforeAndAfter, FlatSpec, Matchers}
import tcompiler.{Main, TestUtils}
import tcompiler.analyzer.Symbols.{ClassSymbol, MethodSymbol, VariableSymbol}
import tcompiler.analyzer.Types._
import tcompiler.analyzer.{NameAnalysis, TypeChecker, TypeChecking}
import tcompiler.ast.Trees._
import tcompiler.ast.{Parser, Printer}
import tcompiler.imports.ImportMap
import tcompiler.lexer.Lexer
import tcompiler.modification.Templates
import tcompiler.utils.{Context, Pipeline, Reporter}
import tcompiler.utils.Extensions._

import scala.util.Random

/**
  * Created by Tim Lindeberg on 4/2/2016.
  */
class OperatorCodeSpec extends FlatSpec with Matchers with BeforeAndAfter {

  val Flag = "--eval"

  val TestFolder           = "./tmpTest"
  val TestFilePath: String = TestFolder + "/tmpTestFile.kool"

  var testFolderFile = new File(TestFolder)
  var testFile       = new File(TestFilePath)

  val Compiler  : Pipeline[List[File], Unit] = Lexer andThen Parser andThen Templates andThen NameAnalysis andThen TypeChecking andThen CodeGeneration
  val Rand                                   = new Random()
  val TestCtx                                = Context(reporter = new Reporter(suppressWarnings = true), files = List(testFile), outDirs = List(testFolderFile))
  val TestImportMap                          = new ImportMap(TestCtx)
  val TypeCheckCtx                           = Context(reporter = new Reporter(suppressWarnings = true), files = List(testFile))
  val ClassSymbol                            = new ClassSymbol("obj", false)
  val MainMethod: MethodSymbol               = new MethodSymbol("main", ClassSymbol, None, Set(Public(), Static())).setType(TUnit)
  val TypeChecker                            = new TypeChecker(TypeCheckCtx, TestImportMap, MainMethod)


  val MainName      = "Main"
  val IdName        = "x"
  val StringLength  = 5
  val NumberOfTests = 1

  val int   : () => IntLit                             = () => IntLit(Rand.nextInt).setType(Int)
  val bool  : () => Literal[Boolean] with Serializable = () => (if (Rand.nextBoolean) TrueLit() else FalseLit()).setType(Bool)
  val long  : () => LongLit                            = () => LongLit(Rand.nextLong % scala.Int.MaxValue).setType(Long)
  val float : () => FloatLit                           = () => FloatLit(Rand.nextFloat).setType(Float)
  val double: () => DoubleLit                          = () => DoubleLit(Rand.nextDouble % scala.Float.MaxValue).setType(Double)
  val char  : () => CharLit                            = () => CharLit(randChar).setType(Char)
  val string: () => StringLit                          = () => StringLit(Rand.nextString(StringLength)).setType(String)

  val types       : List[() => ExprTree]                   = List[() => ExprTree](int, bool, long, float, double, char, string)
  val combinations: List[(() => ExprTree, () => ExprTree)] = for (x <- types; y <- types) yield (x, y)


  before {
    testFolderFile.mkdir()
  }

  after {
    testFile.delete()
    testFolderFile.listFiles.foreach(_.delete())
    testFolderFile.delete()
  }

  //  behavior of s"RandomTesting ($NumberOfTests x)"
  //
  //  it should "Plus" in testOperator(Plus)
  //  it should "Minus" in testOperator(Minus)
  //  it should "Times" in testOperator(Times)
  //  it should "Div" in testOperator(Div)
  //  it should "Mod" in testOperator(Modulo)
  //
  //  it should "LogicAnd" in testOperator(LogicAnd)
  //  it should "LogicOr" in testOperator(LogicOr)
  //  it should "LogicXor" in testOperator(LogicXor)
  //
  //  it should "LeftShift" in testOperator(LeftShift)
  //  it should "RightShift" in testOperator(RightShift)
  //
  //  it should "Assign" in testAssignmentOperator(Assign)
  //  it should "ArrayAssign" in testArrayAssignmentOperator(ArrayAssign)
  //
  //  it should "LessThan" in testOperator(LessThan)
  //  it should "LessThanEquals" in testOperator(LessThanEquals)
  //  it should "GreaterThan" in testOperator(GreaterThan)
  //  it should "GreaterThanEquals" in testOperator(GreaterThanEquals)
  //
  //  ignore should "Equals" in testOperator(Equals)
  //  ignore should "NotEquals" in testOperator(NotEquals)


  def testOperator(operator: (ExprTree, ExprTree) => ExprTree): Unit = {
    combinations.foreach { case (lhs, rhs) =>
      val expr = operator(lhs(), rhs())

      if (exprTypeChecks(expr)) {
        NumberOfTests times testExpression(expr)
      }
    }
  }

  def testAssignmentOperator(operator: (VariableID, ExprTree) => ExprTree): Unit =
    combinations.foreach { case (lhs, rhs) =>
      val tpe = lhs().getType
      val id = VariableID(IdName).setSymbol(new VariableSymbol(IdName)).setType(tpe)
      id.setType(tpe)
      def expr = () => operator(id, rhs())
      if (exprTypeChecks(expr())) {
        NumberOfTests times testAssignment(tpe, expr())
      }
    }

  def testArrayAssignmentOperator(operator: (VariableID, ExprTree, ExprTree) => ExprTree): Unit =
    combinations.foreach { case (lhs, rhs) =>
      val tpe = lhs().getType
      val id = VariableID(IdName).setSymbol(new VariableSymbol(IdName)).setType(TArray(tpe))
      def expr = () => operator(id, IntLit(0), rhs())
      println(tpe + ",  " + rhs().getType)
      if (exprTypeChecks(expr())) {
        NumberOfTests times testArrayAssignment(tpe, expr())
      }
    }

  private def scalaVariableDeclaration(tpe: Type) = {
    val scalaType = tpe match {
      case Bool => "Boolean"
      case _    => tpe.toString
    }

    val defaultValue = tpe match {
      case _: TInt | _: TLong | _: TFloat | _: TDouble | _: TChar => "0"
      case _: TBool                                               => "false"
      case _                                                      => ???
    }
    s"var $IdName: $scalaType = $defaultValue"
  }

  private def scalaArrayDeclaration(tpe: Type) = {
    val scalaType = tpe match {
      case Bool => "Boolean"
      case _    => tpe.toString
    }

    s"var $IdName: Array[$scalaType] = new Array[$scalaType](1)"
  }

  private def exprTypeChecks(expr: ExprTree) = TypeChecker.tcExpr(expr) match {
    case TError                               => false
    case _ if TypeCheckCtx.reporter.hasErrors =>
      TypeCheckCtx.reporter.clear()
      false
    case _                                    =>
      true
  }

  private def testExpression(expr: ExprTree) = {
    val operation = Printer(expr)
    println("Testing " + operation)
    val scalaRes = getScalaResult(operation)
    val res = getResult(operatorProgram(operation))
    assert(res == scalaRes, s" for expression $operation.\n\tScala:  '$scalaRes'\n\tResult: '$res'")
  }

  private def testAssignment(tpe: Type, expr: ExprTree) = {
    TestUtils.Interpreter.interpret(scalaVariableDeclaration(tpe))
    val operation = Printer(expr)
    println(s"Testing $operation ($IdName : $tpe)")
    val scalaRes = getScalaResult(operation).trim + System.lineSeparator() + getScalaResult(IdName).trim
    if (!scalaRes.contains("error")) {
      val res = getResult(assignmentProgram(operation, tpe)).trim
      assertResult(operation + " (x: " + tpe + ")", res, scalaRes)
    }

  }

  private def testArrayAssignment(tpe: Type, expr: ExprTree) = {
    TestUtils.Interpreter.interpret(scalaArrayDeclaration(tpe))
    val operation = Printer(expr)
    println(s"Testing $operation ($IdName : $tpe[])")
    val scalaRes = getScalaResult(IdName + "(0)")
    if (!scalaRes.contains("error")) {
      val res = getResult(arrayAssignmentProgram(operation, tpe))
      assertResult(operation + " (x: " + tpe + ")", res, scalaRes)
    }
  }

  private def assertResult(operation: String, res: String, scalaRes: String) =
    assert(res == scalaRes, s" for expression $operation.\n\tScala:  '$scalaRes'\n\tResult: '$res'")

  private def operatorProgram(operation: String) = s"println($operation)"

  private def assignmentProgram(operation: String, tpe: Type) =
    s"""
var $IdName: $tpe
println($operation)
println($IdName)
     """

  private def arrayAssignmentProgram(operation: String, tpe: Type) =
    s"""
var $IdName: $tpe[] = new $tpe[1]
println($operation)
println($IdName[0])
     """


  private def getResult(program: String) = {
    setTestProgram(program)
    Compiler.run(TestCtx)(TestCtx.files)
    val mainName = TestCtx.files.head.getName.dropRight(Main.FileEnding.length)
    TestUtils.executeTProgram(List(TestFolder, Main.TDirectory), mainName).trim
  }

  private def getScalaResult(operation: String) = {
    val r = TestUtils.Interpreter.interpret(operation)
    if (r.contains("error")) "error"
    else r.split("=").last.trim
  }

  private def setTestProgram(program: String) = {
    var out: FileWriter = null
    try {
      out = new FileWriter(TestFilePath)
      out.write(program)
    } catch {
      case e: IOException => e.printStackTrace()
    } finally {
      if (out != null)
        out.close()
    }
  }

  private def randChar = {
    val illegalChars = List[Char]('\'')
    var c: Char = 0
    do {
      c = Rand.nextPrintableChar()
    } while (illegalChars.contains(c))
    c
  }
}
