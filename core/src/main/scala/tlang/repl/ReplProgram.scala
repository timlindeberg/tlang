package tlang.repl

import java.io.File
import java.nio.file.Files

import cafebabe.StackTrace
import tlang.compiler.analyzer.Symbols.ClassSymbol
import tlang.compiler.analyzer.Types.TUnit
import tlang.compiler.analyzer.{FlowAnalysis, NameAnalysis, TypeChecking}
import tlang.compiler.ast.Trees._
import tlang.compiler.ast.{Parser, PrettyPrinter, Trees}
import tlang.compiler.code.{CodeGeneration, Desugaring, TreeBuilder}
import tlang.compiler.error.SyntaxHighlighter
import tlang.compiler.imports.ImportMap
import tlang.compiler.lexer.Lexer
import tlang.compiler.modification.Templates
import tlang.compiler.{Context, Pipeline}
import tlang.utils.Extensions._
import tlang.utils.{Colors, ProgramExecutor, Source, StringSource}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.concurrent.duration

/**
  * Created by Tim Lindeberg on 2/14/2017.
  */
case class ReplProgram(ctx: Context) {

  import ctx.formatting.colors._

  private val TempDir: File = Files.createTempDirectory("repl").toFile
  TempDir.deleteOnExit()

  private val ClassName        = "ReplClass"
  private val ReplOutputPrefix = "$ReplRes$"
  private val PrintPrefix      = Print(StringLit(ReplOutputPrefix))
  private val ClassFile: File  = new File(TempDir.getAbsolutePath + File.separator + ClassName + ".class")
  private val OutDirectories   = Set(TempDir)
  private val Ctx              = ctx.copy(outDirs = OutDirectories)

  private val ReplClassID        = ClassID(ClassName)
  private val ReplClassSymbol    = new ClassSymbol(ClassName, isAbstract = false)
  private val ResultVariableName = "res"

  private val printer                 = PrettyPrinter(Colors(isActive = true))
  private val maxDuration             = duration.Duration(5, "sec")
  private val programExecutor         = ProgramExecutor(maxDuration)
  private val treeBuilder             = TreeBuilder()
  private val newStatementTransformer = new NewStatementTransformer()
  private val syntaxHighlighter       = SyntaxHighlighter(ctx.formatting.colors)

  private val parse   : Pipeline[Source, CompilationUnit]          = Lexer andThen Parser
  private val frontEnd: Pipeline[CompilationUnit, CompilationUnit] =
    Templates andThen NameAnalysis andThen TypeChecking andThen FlowAnalysis
  private val compile : Pipeline[CompilationUnit, StackTrace]      = Desugaring andThen CodeGeneration

  private val classes      : mutable.Map[String, ClassDeclTree]  = mutable.Map()
  private val fields       : mutable.Map[String, VarDecl]        = mutable.Map()
  private val methods      : mutable.Map[String, MethodDeclTree] = mutable.Map()
  private var history      : ListBuffer[StatTree]                = ListBuffer()
  private var newStatements: List[StatTree]                      = Nil
  private var resultCounter                                      = 0

  def execute(command: String): List[String] = {
    history ++= newStatements.filter(stat => !(stat.isInstanceOf[Print] || stat.isInstanceOf[Println]))
    newStatements = Nil


    val definitionMessages = extractDefinitionsFromInput(command)

    compile.run(Ctx)(generateCompilationUnit() :: Nil)

    val executionMessages = programExecutor(ClassFile) match {
      case Some(res) =>
        res.lines
          .filter(_.startsWith(ReplOutputPrefix))
          .map(_.drop(ReplOutputPrefix.length))
          .toList
      case None      => List(s"Execution timed out after $maxDuration.")
    }
    definitionMessages ++ executionMessages
  }

  def prettyPrinted: String = printer(generateCompilationUnit())

  private def generateCompilationUnit(): CompilationUnit = {
    var stats = history.toList
    if (newStatements != Nil) {
      val newStats = Block(newStatements)
      stats = stats :+ newStats
    }

    val block = Some(Block(stats))
    val mainMethod = MethodDeclTree.mainMethod(block, ReplClassSymbol)
    val mainClass = ClassDecl(ReplClassID, Nil, fields.values.toList, mainMethod :: methods.values.toList).setSymbol(ReplClassSymbol)
    val cu = CompilationUnit(Package(Nil), mainClass :: classes.values.toList, new ImportMap(Ctx))
    val analyzed = frontEnd.run(Ctx)(cu :: Nil).head
    newStatementTransformer(analyzed).prettyPrint
  }

  private def parseInput(command: String) = {
    val input = StringSource(command, ClassName) :: Nil
    parse.run(Ctx)(input).head
  }

  // Updates the internal state of the Repl Class and returns messages for all new
  // definitions
  private def extractDefinitionsFromInput(command: String): List[String] = {
    val parsedInput = parseInput(command)

    val mainClass = parsedInput.classes.find {_.tpe == ReplClassID}
    mainClass match {
      case Some(mainClass) =>
        val classes = parsedInput.classes.remove(mainClass)
        val (methods, stats) = mainClass.methods.find(_.isMain) match {
          case Some(mainMethod) => (mainClass.methods.remove(mainMethod), mainMethod.stat)
          case None             => (mainClass.methods, None)
        }

        extractClasses(classes) ++ extractMethods(methods) ++ extractStatements(stats)
      case None            => extractClasses(parsedInput.classes)
    }
  }

  private def extractClasses(newClasses: List[ClassDeclTree]): List[String] = {
    classes ++= newClasses.map(clazz => clazz.tpe.name -> clazz)
    newClasses.map { clazz =>
      "Defined " + KeywordColor("class ") + VarColor(clazz.tpe.toString)
    }
  }

  private def extractMethods(newMethods: List[MethodDeclTree]): List[String] = {
    methods ++= newMethods.map(meth => meth.signature -> meth)
    newMethods.map { meth =>
      "Defined " + KeywordColor("method ") + syntaxHighlighter(meth.signature)
    }
  }

  private def extractStatements(stat: Option[StatTree]): List[String] = {
    if (stat.isEmpty)
      return Nil

    val stats = stat.get match {
      case Block(stats) => stats
      case s            => s :: Nil
    }

    newStatements = stats
    stats.filterInstance[VarDecl].map { variable =>
      "Defined " + KeywordColor("variable ") + VarColor(variable.id.name)
    }
  }

  // Adds a variable declaration and print statement for each of the entered
  // expressions
  private class NewStatementTransformer extends Trees.Transformer {

    // This could potentially transform other code as well
    override protected def _transform(t: Tree): Tree = t match {
      case block@Block(stats) if stats.nonEmpty =>
        stats.last match {
          case Block(newStats) =>
            newStatements = newStats.flatMap(convert)
            Block(stats.dropRight(1) ++ newStatements)
          case _               => block
        }
      case _                                    => super._transform(t)
    }

    private def convert(statTree: StatTree): List[StatTree] = statTree match {
      case Block(stats)                 => Block(stats.flatMap(convert)) :: Nil
      case acc@Access(_, _: MethodCall) => if (acc.getType == TUnit) acc :: Nil else saveAndPrint(acc)
      case UselessStatement(e)          => saveAndPrint(e)
      case p: Println                   => PrintPrefix :: p :: Nil
      case p: Print                     => PrintPrefix :: p :: Nil
      case _                            => statTree :: Nil
    }

    private def saveAndPrint(e: ExprTree) = {
      val varName = ResultVariableName + resultCounter
      resultCounter += 1

      val varDecl = treeBuilder.createVarDecl(varName, e, prependDollar = false)

      // Prints the prefix so we know to parse it later as well as a variable declaration message
      val stats = varDecl :: PrintPrefix :: Print(StringLit(varName + " = ")) :: Println(varDecl.id) :: Nil
      stats
    }

  }

}
