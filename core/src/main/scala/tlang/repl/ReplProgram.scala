package tlang.repl

import java.io.File
import java.nio.file.Files

import cafebabe.StackTrace
import tlang.compiler.analyzer.Symbols.ClassSymbol
import tlang.compiler.analyzer.{FlowAnalysis, NameAnalysis, TypeChecking}
import tlang.compiler.ast.Parser
import tlang.compiler.ast.Trees._
import tlang.compiler.code.{CodeGeneration, Desugaring, TreeBuilder}
import tlang.compiler.error.Boxes.Light
import tlang.compiler.error.{DefaultReporter, Formatting}
import tlang.compiler.imports.ImportMap
import tlang.compiler.lexer.Lexer
import tlang.compiler.modification.Templates
import tlang.compiler.{Context, Pipeline}
import tlang.utils.Extensions._
import tlang.utils.{Colors, ProgramExecutor, Source, StringSource}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
  * Created by Tim Lindeberg on 2/14/2017.
  */
case class ReplProgram() {

  val TempDir: File = Files.createTempDirectory("repl").toFile
  TempDir.deleteOnExit()

  val ClassName       = "ReplClass"
  val ClassFile: File = new File(TempDir.getAbsolutePath + File.separator + ClassName + ".class")
  val OutDirectories  = Set(TempDir)
  val Ctx             = Context(
    reporter = DefaultReporter(formatting = Formatting(Light, 80, Colors(isActive = true))),
    outDirs = OutDirectories
  )

  val ReplClassID        = ClassID(ClassName)
  val ReplClassSymbol    = new ClassSymbol(ClassName, isAbstract = false)
  val ResultVariableName = "res"


  val programExecutor = ProgramExecutor()
  val treeBuilder     = TreeBuilder()

  val parse  : Pipeline[Source, CompilationUnit]     = Lexer andThen Parser
  val frontEnd                                       = Templates andThen NameAnalysis andThen TypeChecking andThen
    FlowAnalysis andThen Desugaring
  val compile: Pipeline[CompilationUnit, StackTrace] =
    Templates andThen NameAnalysis andThen TypeChecking andThen
      FlowAnalysis andThen Desugaring andThen CodeGeneration

  private val classes   : mutable.Map[String, ClassDeclTree]  = mutable.Map()
  private val fields    : mutable.Map[String, VarDecl]        = mutable.Map()
  private val methods   : mutable.Map[String, MethodDeclTree] = mutable.Map()
  private var history   : ListBuffer[StatTree]                = ListBuffer()
  private var statements: List[StatTree]                      = Nil
  private var resultCounter                                   = 0

  def execute(command: String): List[String] = {
    history ++= statements.filter(stat => !(stat.isInstanceOf[Print] || stat.isInstanceOf[Println]))

    val cu = parseCommand(command)
    if (history.isEmpty)
      return Nil

    compile.run(Ctx)(cu :: Nil)

    programExecutor(ClassFile) match {
      case Some(res) => List(res)
      case None      => List("Timed out!")
    }
  }

  def generateCompilationUnit: CompilationUnit = {
    val block = Some(Block(history.toList ++ statements))
    val mainMethod = MethodDeclTree.mainMethod(block, ReplClassSymbol)
    val mainClass = ClassDecl(ReplClassID, Nil, fields.values.toList, mainMethod :: methods.values.toList).setSymbol(ReplClassSymbol)
    CompilationUnit(Package(Nil), mainClass :: classes.values.toList, new ImportMap(Ctx))
  }

  private def parseCommand(command: String): CompilationUnit = {
    val input = StringSource(command, ClassName) :: Nil
    val parsedInput = parse.run(Ctx)(input).head

    val mainClass = getMainClass(parsedInput)
    mainClass match {
      case Some(clazz) =>
        val mainMethod = clazz.methods.find(_.isMain).get
        classes ++= parsedInput.classes.remove(clazz).map(clazz => clazz.tpe.name -> clazz)
        methods ++= clazz.methods.remove(mainMethod).map(meth => meth.signature -> meth)
        mainMethod.stat.ifDefined { stat => statements = unBlock(stat) }
      case None        =>
        classes ++= parsedInput.classes.map(clazz => clazz.tpe.name -> clazz)
    }
    val newCu = generateCompilationUnit
    frontEnd.run(Ctx)(newCu :: Nil).head
    newCu

  }

  private def unBlock(statTree: StatTree): List[StatTree] = statTree match {
    case Block(stats) => stats
    case s            => s :: Nil
  }

  private def getMainClass(compilationUnit: CompilationUnit) = compilationUnit.classes.find {_.tpe == ReplClassID}
  /*
    private def convertStats(cu: CompilationUnit): CompilationUnit = {
      val mainClass = getMainClass(cu).get
      val mainMethod = mainClass.methods.find(_.isMain).get


    }

    private def convert(statTree: StatTree) = statTree match {
      case Block(stmts)                 => stmts.flatMap(convert)
      case acc@Access(_, _: MethodCall) => if (acc.getType == TUnit) acc :: Nil else saveAndPrint(acc)
      case UselessStatement(e)          => saveAndPrint(e)
      case p: Println                   => p :: Nil
      case p: Print                     => p :: Nil
      case _                            => stat :: Nil
    }
    private def saveAndPrint(e: ExprTree) = {
      val stats = treeBuilder.createVarDecl(ResultVariableName + resultCounter, e) :: Println(e) :: Nil
      resultCounter += 1
      stats
    }
  */
}
