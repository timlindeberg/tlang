package tlang.repl.evaluation

import tlang.compiler.analyzer.Symbols.ClassSymbol
import tlang.compiler.ast.PrettyPrinter
import tlang.compiler.ast.Trees._
import tlang.compiler.imports.Imports
import tlang.utils.Extensions._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

case class ReplState(prettyPrinter: PrettyPrinter, private val _imports: Imports) {

  import Evaluator._

  private var _newStatements = List[StatTree]()
  private val _history       = ListBuffer[StatTree]()

  private val _classes = mutable.Map[String, ClassDeclTree]()
  private val _methods = mutable.Map[String, MethodDeclTree]()

  def history: List[StatTree] = _history.toList
  def newStatements: List[StatTree] = _newStatements
  def classes: List[ClassDeclTree] = _classes.values.toList
  def methods: List[MethodDeclTree] = _methods.values.toList
  def imports: Imports = _imports

  def compilationUnit: CompilationUnit = {
    val classSymbol = new ClassSymbol(ClassName)

    val mainMethod = MethodDeclTree.mainMethod(Some(mainStatements), classSymbol)
    val mainClass = ClassDecl(ReplClassID, Nil, Nil, mainMethod :: methods).setSymbol(classSymbol)
    val allClasses = mainClass :: classes
    CompilationUnit(Package(Nil), allClasses, imports)
  }

  def prettyPrinted: String = {
    clearStatements()
    prettyPrinter(compilationUnit :: Nil).trimWhiteSpaces
  }

  def clearStatements(): this.type = {
    _newStatements = Nil
    this
  }

  def setNewStatements(stats: List[StatTree]): this.type = {
    _newStatements = stats
    this
  }

  def addClasses(newClasses: Traversable[ClassDeclTree]): this.type = {
    _classes ++= newClasses.map(clazz => clazz.tpe.toString -> clazz)
    this
  }

  def addMethods(newMethods: Traversable[MethodDeclTree]): this.type = {
    _methods ++= newMethods.map(meth => meth.signature -> meth)
    this
  }

  def addImports(newImports: Imports): this.type = {
    imports ++= newImports
    this
  }

  def addStatementsToHistory(): this.type = {
    _history ++= _newStatements.filter(stat => !(stat.isInstanceOf[Print] || stat.isInstanceOf[Println]))
    this
  }

  private def mainStatements: Block = {
    if (_newStatements == Nil)
      return Block(history)

    val newStats = Block(PrintMarker :: (_newStatements :+ PrintMarker))
    Block(history :+ newStats)
  }

}
