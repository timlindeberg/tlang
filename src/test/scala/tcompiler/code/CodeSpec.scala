package tcompiler.code

import java.io._

import org.scalatest._
import tcompiler.TestUtils
import tcompiler.analyzer.Symbols.{ClassSymbol, MethodSymbol, VariableSymbol}
import tcompiler.analyzer.Types._
import tcompiler.analyzer.{NameAnalysis, Symbols, TypeChecker, TypeChecking}
import tcompiler.ast.Trees._
import tcompiler.ast._
import tcompiler.lexer.Lexer
import tcompiler.modification.{Imports, Templates}
import tcompiler.utils.{Context, Reporter}

import scala.io.Source
import scala.sys.process._
import scala.util.Random

class CodeSpec extends FlatSpec with Matchers with BeforeAndAfter {
  val flag = "--eval"

  val Compiler = Lexer andThen Parser andThen Templates andThen Imports andThen NameAnalysis andThen TypeChecking andThen CodeGeneration

  val TestFolder   = "./tmpTest"
  val TestFilePath = TestFolder + "/tmpTestFile.txt"

  var testFolderFile = new File(TestFolder)
  var testFile       = new File(TestFilePath)
  val TestCtx        = new Context(reporter = new Reporter(quiet = false), file = testFile, outDir = Some(new File(TestFolder + "/")))
  val TypeCheckCtx   = new Context(reporter = new Reporter(quiet = true), file = testFile, outDir = None)
  val ClassSymbol    = new ClassSymbol("obj")
  val MainMethod     = new MethodSymbol("main", ClassSymbol, Private).setType(TUnit)
  val TypeChecker    = new TypeChecker(TypeCheckCtx, MainMethod)

  val MainName      = "Main"
  val IdName        = "x"
  val Rand          = new Random()
  val StringLength  = 5
  val NumberOfTests = 5

  implicit def intWithTimes(n: Int) = new {
    def times(f: => Unit): Unit = 1 to n foreach { _ => f }
  }


  before {
    testFolderFile.mkdir()
    Symbols.ID.reset
  }

  after {
    testFile.delete()
    testFolderFile.listFiles.foreach(_.delete())
    testFolderFile.delete()
  }

  behavior of "RandomTesting"

  //  it should "Plus" in testOperator(Plus)
  //  it should "Minus" in testOperator(Minus)
  //  it should "Times" in testOperator(Times)
  //  it should "Div" in testOperator(Div)
  //  it should "Mod" in testOperator(Modulo)
  //
  //  it should "LogicAnd" in testOperator(LogicAnd)
  //  it should "LogicOr" in testOperator(LogicOr)
  //  it should "LogicXor" in testOperator(LogicXor)

//  it should "LeftShift" in testOperator(LeftShift)
//  it should "RightShift" in testOperator(RightShift)

  it should "Assign" in testAssignmentOperator(Assign)

  //behavior of "Programs"
  //TestUtils.programFiles(TestUtils.resources + "programs").foreach(testFile(_))
  //TestUtils.programFiles(TestUtils.resources + "given/programs").foreach(testFile(_))

  def testFile(f: File): Unit = {
    if (f.isDirectory)
      f.listFiles.foreach(testFile(_))
    else
      it should "code gen program " + f.toPath in test(f)
  }


  val int    = () => IntLit(Rand.nextInt).setType(TInt)
  val bool   = () => (if (Rand.nextBoolean) True() else False()).setType(TBool)
  val long   = () => LongLit(Rand.nextLong).setType(TLong)
  val float  = () => FloatLit(Rand.nextFloat).setType(TFloat)
  val double = () => DoubleLit(Rand.nextDouble).setType(TDouble)
  val char   = () => CharLit(Rand.nextPrintableChar()).setType(TChar)
  val string = () => StringLit(Rand.nextString(StringLength)).setType(TString)

  val types        = List[() => ExprTree](int, bool, long, float, double, char, string)
  val combinations = for (x <- types; y <- types) yield (x, y)


  def testOperator(operator: (ExprTree, ExprTree) => ExprTree) = {
    combinations.foreach { case (lhs, rhs) =>
      val expr = operator(lhs(), rhs())

      if (exprTypeChecks(expr)) {
        NumberOfTests times testExpression(operator(lhs(), rhs()))
      }
    }
  }

  def testAssignmentOperator(operator: (Identifier, ExprTree) => ExprTree) =
    combinations.foreach { case (lhs, rhs) =>
      val tpe = lhs().getType
      val id = Identifier(IdName).setSymbol(new VariableSymbol(IdName)).setType(tpe)
      def expr = () => operator(id, rhs())
      TestUtils.Interpreter.interpret(scalaVariableDelcaration(tpe))
      if (exprTypeChecks(expr())) {
        NumberOfTests times testAssignment(tpe, expr())
      }
    }

  def scalaVariableDelcaration(tpe: Type) = {
    val scalaType = tpe match {
      case TBool => "Boolean"
      case _     => tpe.toString
    }

    val defaultValue = tpe match {
      case TInt | TLong | TFloat | TDouble | TChar => "0"
      case TBool                                   => "true"
      case TString                                 => "\"\""
    }
    s"var $IdName: $scalaType = $defaultValue"
  }


  def exprTypeChecks(expr: ExprTree) = TypeChecker.tcExpr(expr) match {
    case TError                               => false
    case _ if TypeCheckCtx.reporter.hasErrors =>
      TypeCheckCtx.reporter.clearErrors()
      false
    case _                                    =>
      true
  }

  def testExpression(expr: ExprTree) = {
    val operation = Printer(expr)
    println("Testing " + operation)
    val scalaRes = getScalaResult(operation)
    val res = getResult(operatorProgram(operation))
    assert(res == scalaRes, s" for expression $operation.\n\tScala:  '$scalaRes'\n\tResult: '$res'")
  }

  def testAssignment(tpe: Type, expr: ExprTree) = {
    val operation = Printer(expr) // remove ; newline
    println("Testing " + operation + " (x: " + tpe + ")")
    getScalaResult(operation)
    val scalaRes = getScalaResult(IdName)
    println("scalaRes: " + scalaRes)
    val res = getResult(assignmentProgram(operation, tpe))
    assertResult(operation, res, scalaRes)
  }

  def assertResult(operation: String, res: String, scalaRes: String) =
    assert(res == scalaRes, s" for expression $operation.\n\tScala:  '$scalaRes'\n\tResult: '$res'")

  def operatorProgram(operation: String) = s"main $MainName = { println($operation); }"

  def assignmentProgram(operation: String, tpe: Type) =
    s"""
main $MainName = { new A().B(); }

class A {
  var $IdName: $tpe;
  Def B(): Unit = { $operation; println($IdName); }
}
     """


  def getResult(program: String) = {
    setTestProgram(program)
    Compiler.run(TestCtx)(TestCtx.file)

    execute(TestCtx.outDir.get, MainName, "").trim
  }

  def getScalaResult(operation: String) = TestUtils.Interpreter.interpret(operation).split("=").last.trim

  def setTestProgram(program: String) = {
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

  def test(file: File, exception: Boolean = false) = {
    val ctx = new Context(reporter = new tcompiler.utils.Reporter, file = file, outDir = Some(new File("./gen/" + file.getName + "/")))
    val program = (Lexer andThen Parser andThen NameAnalysis andThen TypeChecking).run(ctx)(ctx.file)

    // println(Printer(program))
    TestUtils.HasTypes(program) should be(true)
    ctx.reporter.hasErrors should be(false)

    CodeGeneration.run(ctx)(program)

    val res = execute(file, program.main.get.id.value, "./gen/")
    // Try and compare result with solution file
    try {
      val sol = readSolution(file + "-solution").toList
      //println("res: \n" + res)
      //println("sol: \n" + sol.mkString("\n"))
      val r = TestUtils.lines(res)
      r.length should be(sol.length)
      r.zip(sol).foreach { case (res, sol) => res.trim should be(sol.trim) }
    } catch {
      case t: FileNotFoundException =>
    }

    // res should be (getAnswer(file))

  }

  def flatten(l: List[_]): List[_] = l flatMap {
    case l1: List[_] => flatten(l1)
    case otherwise   => List(otherwise)
  }

  def execute(f: File, main: String, prefix: String): String = "java -cp " + prefix + f.getName + " " + main !!

  def getAnswer(file: File) = Seq(TestUtils.runScript, flag + " " + file.toPath) !!
  def readSolution(fileName: String): Iterator[String] = Source.fromFile(fileName).getLines()

}