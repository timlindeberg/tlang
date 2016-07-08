package tcompiler.code

import tcompiler.analyzer.Symbols.{ClassSymbol, MethodSymbol, VariableSymbol}
import tcompiler.analyzer.Types._
import tcompiler.ast.Trees._
import tcompiler.utils.Positioned

import scala.collection.mutable.ListBuffer

/**
  * Created by Tim Lindeberg on 7/5/2016.
  */

class TreeBuilder {
  val code = ListBuffer[StatTree]()

  def put(stat: StatTree) = {
    stat foreach {
      case t: Typed if t.getType == TUntyped =>
        sys.error(s"Tree $t does not have a type!")
      case _                                 =>
    }
    code += stat
  }

  def putVarDecl(idName: String, initExpression: ExprTree): VariableID = {
    val decl = createVarDecl(idName, initExpression)
    code += decl
    decl.id
  }

  def createMethodCall(obj: ExprTree, classSymbol: ClassSymbol, methName: String, args: List[Type] = List()): NormalAccess = {
    val methodSymbol = classSymbol.lookupMethod(methName, args).get
    createMethodCall(obj, methodSymbol)
  }

  def createMethodCall(obj: ExprTree, name: String, tpe: Type): NormalAccess =
    createMethodCall(obj, createMethodSymbol(name, tpe))

  def createMethodCall(obj: ExprTree, methodSymbol: MethodSymbol): NormalAccess = createMethodCall(obj, methodSymbol, List())

  def createMethodCall(obj: ExprTree, methodSymbol: MethodSymbol, args: ExprTree*): NormalAccess =
    createMethodCall(obj, methodSymbol, args.toList)

  def createMethodCall(obj: ExprTree, methodSymbol: MethodSymbol, args: List[ExprTree]): NormalAccess = {
    val tpe = methodSymbol.getType
    val sizeMethId = createMethodId(methodSymbol)
    val mCall = MethodCall(sizeMethId, args).setType(tpe)
    NormalAccess(obj, mCall).setType(tpe)
  }

  private def createMethodSymbol(name: String, tpe: Type) =
    new MethodSymbol(name, new ClassSymbol("", false), None, Set()).setType(tpe)

  private def createMethodId(methodSymbol: MethodSymbol): MethodID = MethodID(methodSymbol.name).setSymbol(methodSymbol)

  def createVarDecl(idName: String, initExpression: ExprTree): VarDecl = {
    val modifiers = scala.collection.immutable.Set[Modifier](Private())
    val tpe = initExpression.getType
    if (tpe == TUntyped)
      sys.error("Cannot create var decl from an untyped initial expression.")

    initExpression.setType(tpe)
    val name = '$' + idName
    val id = VariableID(name)
    val varDecl = VarDecl(None, id, Some(initExpression), modifiers)
    val symbol = new VariableSymbol(idName)
    symbol.setType(tpe)
    varDecl.setSymbol(symbol)
    id.setSymbol(symbol)
    id.setType(tpe)
    varDecl
  }

  def createOne(tpe: Type): ExprTree = tpe match {
    case Int    => IntLit(1)
    case Char   => IntLit(1)
    case Long   => LongLit(1l)
    case Float  => FloatLit(1.0f)
    case Double => DoubleLit(1.0)
    case _       => ???
  }


  def getTypeTree(tpe: Type): TypeTree = tpe match {
    case TUnit                => UnitType()
    case Char                => CharType()
    case Bool                => BooleanType()
    case Int                 => IntType()
    case Long                => LongType()
    case Float               => FloatType()
    case Double              => DoubleType()
    case TArray(t, _)            => ArrayType(getTypeTree(t))
    case TObject(classSymbol, _) => ClassID(classSymbol.name).setSymbol(classSymbol)
    case _                    => ???
  }

  def getCode = {
    val g = GeneratedExpr(code.toList).setPos(code.head)
    code.last match {
      case t: Typed => g.setType(t)
      case _        => g
    }
  }

  def setPos(pos: Positioned) = code.foreach(_.setPos(pos))

}




