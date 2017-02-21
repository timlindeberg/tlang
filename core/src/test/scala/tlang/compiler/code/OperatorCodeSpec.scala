package tlang.compiler.code

import java.io.{File, FileWriter, IOException}

import org.scalatest.{BeforeAndAfter, FlatSpec, Matchers}
import tlang.compiler.analyzer.Symbols.{ClassSymbol, MethodSymbol, VariableSymbol}
import tlang.compiler.analyzer.Types._
import tlang.compiler.analyzer.{NameAnalysis, TypeChecker, TypeChecking}
import tlang.compiler.ast.Trees._
import tlang.compiler.ast.{Parser, PrettyPrinter}
import tlang.compiler.error.DefaultReporter
import tlang.compiler.imports.ImportMap
import tlang.compiler.lexer.Lexer
import tlang.compiler.modification.Templates
import tlang.compiler.{Context, Interpreter}
import tlang.utils.Extensions._
import tlang.utils.{Colors, FileSource, ProgramExecutor}

import scala.util.Random

/**
  * Created by Tim Lindeberg on 4/2/2016.
  */
class OperatorCodeSpec extends FlatSpec with Matchers with BeforeAndAfter {

  private val TestFolder           = "./tmpTest"
  private val TestFilePath: String = TestFolder + "/tmpTestFile.t"

  private val testFolderFile = new File(TestFolder)
  private val testFile       = new File(TestFilePath)

  private val Printer       = PrettyPrinter(Colors(isActive = false))
  private val Compiler      = Lexer andThen Parser andThen Templates andThen NameAnalysis andThen TypeChecking andThen CodeGeneration
  private val Rand          = new Random()
  private val TestCtx       = Context(reporter = DefaultReporter(suppressWarnings = true), files = Set(testFile), outDirs = Set(testFolderFile))
  private val TestImportMap = new ImportMap(TestCtx)
  private val TypeCheckCtx  = Context(reporter = DefaultReporter(suppressWarnings = true), files = Set(testFile))
  private val ClassSymbol   = new ClassSymbol("obj", false)
  private val MainMethod    = new MethodSymbol("main", ClassSymbol, None, Set(Public(), Static())).setType(TUnit)
  private val TypeChecker   = new TypeChecker(TypeCheckCtx, TestImportMap, MainMethod)
  private val Interpreter   = new Interpreter

  private val programExecutor = ProgramExecutor()


  private val IdName        = "x"
  private val StringLength  = 5
  private val NumberOfTests = 1

  private val int    = () => IntLit(Rand.nextInt).setType(Int)
  private val bool   = () => (if (Rand.nextBoolean) TrueLit() else FalseLit()).setType(Bool)
  private val long   = () => LongLit(Rand.nextLong % scala.Int.MaxValue).setType(Long)
  private val float  = () => FloatLit(Rand.nextFloat).setType(Float)
  private val double = () => DoubleLit(Rand.nextDouble % scala.Float.MaxValue).setType(Double)
  private val char   = () => CharLit(randChar).setType(Char)
  private val string = () => StringLit(Rand.nextString(StringLength)).setType(String)

  private val types        = List[() => ExprTree](int, bool, long, float, double, char, string)
  private val combinations = for (x <- types; y <- types) yield (x, y)


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
      case _ if tpe in List(Int, Long, Float, Double, Char) => "0"
      case Bool                                             => "false"
      case _                                                => ???
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
    Interpreter.interpret(scalaVariableDeclaration(tpe))
    val operation = Printer(expr)
    println(s"Testing $operation ($IdName : $tpe)")
    val scalaRes = getScalaResult(operation).trim + System.lineSeparator() + getScalaResult(IdName).trim
    if (!scalaRes.contains("error")) {
      val res = getResult(assignmentProgram(operation, tpe))
      assertResult(operation + " (x: " + tpe + ")", res, scalaRes)
    }

  }

  private def testArrayAssignment(tpe: Type, expr: ExprTree) = {
    Interpreter.interpret(scalaArrayDeclaration(tpe))
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
       |var $IdName: $tpe
       |println($operation)
       |println($IdName)
     """.stripMargin

  private def arrayAssignmentProgram(operation: String, tpe: Type) =
    s"""
       |var $IdName: $tpe[] = new $tpe[1]
       |println($operation)
       |println($IdName[0])
     """.stripMargin

  private def getResult(program: String) = {
    setTestProgram(program)

    val files = TestCtx.files.map(FileSource).toList
    Compiler.run(TestCtx)(files)
    programExecutor(TestCtx, TestCtx.files.head).get
  }

  private def getScalaResult(operation: String) = {
    val r = Interpreter.interpret(operation)
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
