package tlang
package testutils

import tlang.compiler.analyzer.Symbols.{ClassSymbol, ExtensionClassSymbol, MethodSymbol, VariableSymbol}
import tlang.compiler.analyzer.Types
import tlang.compiler.analyzer.Types.{TObject, Type}
import tlang.compiler.ast.Trees._
import tlang.compiler.imports.Imports
import tlang.utils.{Position, Positioned}

import scala.util.Random

trait TreeTesting extends MockitoSugar {

  implicit class PositionedTest[T <: Positioned](pos: T) {
    def setRandomPos: T = pos.setPos(randomPosition)
  }

  val IntType = ClassID(Types.Int.name)
  val StringType = ClassID(Types.String.name)

  private val MaxPositionSize = 100
  private val random = new Random(0)

  implicit def stringToClassId(str: String): ClassID = ClassID(str)
  implicit def stringToVariableId(str: String): VariableID = VariableID(str)
  implicit def stringToMethodId(str: String): MethodID = MethodID(str)
  implicit def valueToOption[T](t: T): Option[T] = Some(t)

  def createVariable(name: String, tpe: Type = Types.Int): VariableID = {
    val pos = randomPosition
    val variable = VariableID(name)
    val sym = new VariableSymbol(name)
    sym.setPos(pos)
    sym.setType(tpe)
    variable.setSymbol(sym)
    variable.setPos(pos)
    variable
  }

  def createIntLit(value: Int): IntLit = IntLit(value).setPos(randomPosition)

  def createClass(
    name: String,
    parents: List[Type] = Nil,
    fields: List[Type] = Nil,
    methods: List[MethodDeclTree] = Nil
  ): ClassDecl = {
    val pos = randomPosition
    val classSymbol = new ClassSymbol(name).setPos(pos)
    methods.foreach { meth => setClassSymbol(meth, classSymbol) }

    val id = ClassID(name).setType(classSymbol.getType).setPos(pos)
    ClassDecl(
      id,
      parents = parents.map { tpe => ClassID(tpe.name).setType(tpe).setRandomPos },
      fields = createVariables(fields) map { variable =>
        val typeTree = ClassID(variable.name).setType(variable.getType).setPos(variable)
        VarDecl(variable, typeTree, None, Set(Public()))
      },
      methods = methods,
      annotations = Nil
    ).setSymbol(classSymbol).setPos(pos)
  }

  def createExtensionClass(
    name: String,
    extendedClass: ClassDecl,
    methods: List[MethodDeclTree] = Nil
  ): ExtensionDecl = {
    val pos = randomPosition
    val classSymbol = new ExtensionClassSymbol(name).setRandomPos
    classSymbol.setExtendedType(extendedClass.getSymbol.getType)
    val tpe = TObject(classSymbol)
    methods.foreach { meth => setClassSymbol(meth, classSymbol) }

    val id = ClassID(name).setType(tpe).setPos(pos)
    ExtensionDecl(
      id,
      ClassID(extendedClass.name).setPos(extendedClass),
      methods = methods,
      annotations = Nil
    ).setSymbol(classSymbol).setPos(pos)
  }

  def createMethod(
    name: String,
    args: List[Type] = Nil,
    retType: Option[Type] = None,
    modifiers: Set[Modifier] = Set(Public()),
    stat: Option[StatTree] = None): MethodDecl = {
    val classSym = new ClassSymbol("MyClass")
    val methSymbol = new MethodSymbol(name, classSym, stat, modifiers).setRandomPos

    val arguments = createVariables(args)
      .map { arg =>
        methSymbol.addArgument(arg.getSymbol)
        val typeTree = ClassID(arg.getType.name).setType(arg.getType).setPos(arg)
        Formal(typeTree, arg).setPos(arg)
      }

    val id = MethodID(name).setSymbol(methSymbol).setPos(methSymbol.getPos)
    val returnType = retType.getOrElse(Types.Int)
    val returnTypeTree = ClassID(returnType.name).setType(returnType)
    MethodDecl(id, modifiers = modifiers, args = arguments, retType = returnTypeTree, stat = stat)
      .setSymbol(methSymbol)
      .setPos(methSymbol.getPos)
  }

  def createVariables(types: List[Type]): List[VariableID] = {
    types
      .zip("abcdefghijklmnopqrstuvxyz")
      .map { case (tpe, name) =>
        val arg = VariableID("" + name).setRandomPos
        val sym = new VariableSymbol("" + name).setPos(arg).setType(tpe)
        arg.setSymbol(sym).setType(tpe)
      }
  }

  def createCU(classes: ClassDecl*)(imports: Imports = mockImports): CompilationUnit = {
    CompilationUnit(Package(), classes.toList, imports)
  }

  def mockImports: Imports = {
    mock[Imports] use { imports =>
      imports.imports returns Nil
    }
  }

  def randomPosition = Position(
    random.nextInt(MaxPositionSize),
    random.nextInt(MaxPositionSize),
    random.nextInt(MaxPositionSize),
    random.nextInt(MaxPositionSize)
  )

  private def setClassSymbol(methodDecl: MethodDeclTree, classSymbol: ClassSymbol) = {
    val sym = methodDecl.getSymbol
    val newSym = new MethodSymbol(sym.name, classSymbol, None, sym.modifiers)
    methodDecl.id.setSymbol(newSym)
    methodDecl.setSymbol(newSym)
  }
}
