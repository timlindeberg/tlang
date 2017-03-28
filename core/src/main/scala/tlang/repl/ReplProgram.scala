package tlang.repl

import java.io.File
import java.lang.reflect.InvocationTargetException

import akka.actor.{Actor, Props}
import tlang.compiler.Context
import tlang.compiler.analyzer.Symbols.ClassSymbol
import tlang.compiler.analyzer.Types._
import tlang.compiler.analyzer.{FlowAnalysis, NameAnalysis, TypeChecking}
import tlang.compiler.ast.Trees._
import tlang.compiler.ast.{Parser, PrettyPrinter, Trees}
import tlang.compiler.code.{CodeGeneration, Desugaring, TreeBuilder}
import tlang.compiler.error.CompilationException
import tlang.compiler.imports.ImportMap
import tlang.compiler.lexer.Lexer
import tlang.compiler.modification.Templates
import tlang.utils.Extensions._
import tlang.utils.{Colors, ProgramExecutor, StringSource}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{CancellationException, TimeoutException}
import scala.util.{Failure, Success}


/**
  * Created by Tim Lindeberg on 2/14/2017.
  */
object ReplProgram {

  trait ReplProgramMessage

  case class Execute(command: String) extends ReplProgramMessage
  case object PrettyPrint extends ReplProgramMessage
  case object StopExecution extends ReplProgramMessage

  def props(ctx: Context, maxOutputLines: Int) =
    Props(new ReplProgram(ctx, maxOutputLines))

  val name = "replProgram"
}

class ReplProgram(ctx: Context, maxOutputLines: Int) extends Actor {


  import ReplProgram._
  import ctx.formatting._
  import ctx.formatting.colors._

  private val ClassName        = "REPL"
  private val ReplOutputMarker = "__ReplRes__"
  private val PrintMarker      = Print(StringLit(ReplOutputMarker))
  private val ClassFile        = new File(ctx.outDirs.head, ClassName + ".class")

  private val ReplClassID             = ClassID(ClassName)
  private val ReplClassSymbol         = new ClassSymbol(ClassName, isAbstract = false)
  private val printer                 = PrettyPrinter(Colors(isActive = true))
  private val programExecutor         = ProgramExecutor()
  private val treeBuilder             = TreeBuilder()
  private val newStatementTransformer = new NewStatementTransformer()
  private val stackTraceHighlighter   = StackTraceHighlighter(ctx.formatting.colors)

  private val parse    = Lexer andThen Parser
  private val frontEnd = Templates andThen NameAnalysis andThen TypeChecking andThen FlowAnalysis
  private val compile  = Desugaring andThen CodeGeneration

  private val classes = mutable.Map[String, ClassDeclTree]()
  private val methods = mutable.Map[String, MethodDeclTree]()
  private val history = ListBuffer[StatTree]()
  private val imports = ImportMap(ctx)

  private var newStatements = List[StatTree]()
  private var resultCounter = 0

  private def parent = context.parent

  private val NoCancel        = () => false
  private var cancelExecution = NoCancel

  // For warmup
  compileAndExecute("val theAnswerToLifeInTheUniverseAndEverything = 21 * 2")


  override def receive: Receive = {
    case Execute(command) => execute(command)
    case StopExecution    => cancelExecution()
    case PrettyPrint      => prettyPrint()
    case _                => ???
  }

  private def execute(command: String): Unit = {
    val (f, cancel) = CancellableFuture { compileAndExecute(command) }
    cancelExecution = cancel
    f onComplete { res =>
      cancelExecution = NoCancel
      val failureColor = Bold + Red
      val message = res match {
        case Success(res) => Renderer.Success(res, truncate = true)
        case Failure(e)   => e match {
          case e: CompilationException      => Renderer.CompileError(e.messages.getErrors)
          case _: TimeoutException          => Renderer.Failure(failureColor("Execution timed out."), truncate = true)
          case _: CancellationException     => Renderer.Failure(failureColor("Execution cancelled."), truncate = true)
          case e: InvocationTargetException => Renderer.Failure(stackTraceHighlighter(e.getCause), truncate = true)
          case e                            => throw e
        }
      }
      newStatements = Nil
      parent ! message
    }
  }


  private def compileAndExecute(command: String): String = {
    val definitionMessages = extractDefinitionsFromInput(command)

    val CU = generateCompilationUnit()
    val analyzed = frontEnd.run(ctx)(CU :: Nil).head

    // Add generated template classes
    classes ++= analyzed.classes
      .filter { clazz => clazz.tpe != ReplClassID && clazz.tpe.toString.contains("$") }
      .map { clazz => clazz.tpe.toString -> clazz }

    val transformed: CompilationUnit = newStatementTransformer(analyzed)
    compile.run(ctx)(transformed :: Nil)

    val res = programExecutor(ClassFile)
    history ++= newStatements.filter(stat => !(stat.isInstanceOf[Print] || stat.isInstanceOf[Println]))

    val executionMessages = getOutput(res).lines.map(colorOutput)

    val sb = new StringBuilder
    definitionMessages.foreach(sb ++= _ + "\n")
    executionMessages.foreach(sb ++= _ + "\n")
    sb.toString.trimWhiteSpaces
  }

  private def prettyPrint(): Unit = {
    newStatements = Nil
    val code = printer(generateCompilationUnit()).trimWhiteSpaces
    parent ! Renderer.Success(code, truncate = false)
  }

  private def getOutput(s: String): String = {
    val start = s.indexOf(ReplOutputMarker)
    val end = s.lastIndexOf(ReplOutputMarker)
    if (start != -1 && end != -1) s.slice(start + ReplOutputMarker.length, end) else ""
  }

  private def colorOutput(s: String): String = {
    if (s.startsWith("val res") && s.contains("=")) {
      val split = s.split("=")
      if (split.length == 2)
        return syntaxHighlighter(split(0)) + "=" + Bold(Green(split(1).rightTrimWhiteSpaces))
    }
    Bold(Green(s.trim))
  }

  private def generateCompilationUnit(): CompilationUnit = {
    var stats = history.toList
    if (newStatements != Nil) {
      val newStats = Block(PrintMarker :: (newStatements :+ PrintMarker))
      stats = stats :+ newStats
    }

    val block = Some(Block(stats))
    val mainMethod = MethodDeclTree.mainMethod(block, ReplClassSymbol)
    val mainClass = ClassDecl(ReplClassID, Nil, Nil, mainMethod :: methods.values.toList).setSymbol(ReplClassSymbol)
    val allClasses = mainClass :: classes.values.toList
    CompilationUnit(Package(Nil), allClasses, imports)
  }

  private def parseInput(command: String) = {
    val input = StringSource(command, ClassName) :: Nil
    parse.run(ctx)(input).head
  }

  // Updates the internal state of the Repl Class and returns messages for all new
  // definitions
  private def extractDefinitionsFromInput(command: String): List[String] = {
    val parsedInput = parseInput(command)

    val mainClass = parsedInput.classes.find { _.tpe == ReplClassID }
    extractImports(parsedInput.importMap) ++ (mainClass match {
      case Some(mainClass) =>
        val classes = parsedInput.classes.remove(mainClass)
        val (methods, stats) = mainClass.methods.find(_.isMain) match {
          case Some(mainMethod) => (mainClass.methods.remove(mainMethod), mainMethod.stat)
          case None             => (mainClass.methods, None)
        }
        extractClasses(classes) ++
          extractMethods(methods) ++
          extractStatements(stats)

      case None => extractClasses(parsedInput.classes)
    })
  }

  private def extractClasses(newClasses: List[ClassDeclTree]): List[String] = {
    classes ++= newClasses.map(clazz => clazz.tpe.toString -> clazz)
    newClasses map { clazz =>
      Bold("Defined ") + KeywordColor("class ") + syntaxHighlighter(clazz.tpe.toString)
    }
  }

  private def extractMethods(newMethods: List[MethodDeclTree]): List[String] = {
    methods ++= newMethods.map(meth => meth.signature -> meth)
    newMethods map { meth =>
      Bold("Defined ") + KeywordColor("method ") + syntaxHighlighter(meth.signature)
    }
  }

  private def extractImports(importMap: ImportMap): List[String] = {
    imports ++= importMap
    importMap.imports map { imp =>
      Bold("Imported ") + syntaxHighlighter(imp.name)
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
    stats.filterInstance[VarDecl] map { variable =>
      Bold("Defined ") + KeywordColor("variable ") + syntaxHighlighter(variable.id.name)
    }
  }

  // Adds a variable declaration and print statement for each of the entered
  // expressions
  private class NewStatementTransformer extends Trees.Transformer {

    // This could potentially transform other code as well
    override protected def _transform(t: Tree): Tree = t match {
      case block@Block(stats) if stats.nonEmpty =>
        stats.last match {
          case Block(newStats) if newStats.length > 2 &&
            newStats.head == PrintMarker &&
            newStats.last == PrintMarker =>
            newStatements = newStats.flatMap(convert)
            Block(stats.dropRight(1) ++ newStatements)
          case _                         => block
        }
      case _                                    => super._transform(t)
    }

    private def convert(statTree: StatTree): List[StatTree] = statTree match {
      case Block(stats)                 => Block(stats flatMap convert) :: Nil
      case acc@Access(_, _: MethodCall) => if (acc.getType == TUnit) acc :: Nil else saveAndPrint(acc)
      case UselessStatement(e)          => saveAndPrint(e)
      case _                            => statTree :: Nil
    }

    private def saveAndPrint(e: ExprTree) = {
      val varName = "res" + resultCounter
      resultCounter += 1

      val varDecl = treeBuilder.createValDecl(varName, e, prefix = "")

      val varDeclMessage = treeBuilder.stringConcat(StringLit(s"val $varName = "), varDecl.id)
      varDecl :: Println(varDeclMessage) :: Nil
    }

  }

}
