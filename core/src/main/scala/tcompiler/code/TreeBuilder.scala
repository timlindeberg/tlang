package tcompiler.code

import tcompiler.analyzer.Symbols.{ClassSymbol, MethodSymbol, VariableSymbol}
import tcompiler.analyzer.Types._
import tcompiler.ast.Trees._
import tcompiler.imports.ImportMap
import tcompiler.utils.Positioned

import scala.collection.mutable.ListBuffer

/**
  * Created by Tim Lindeberg on 7/5/2016.
  */

class TreeBuilder {
  val code = ListBuffer[StatTree]()

  def put(stat: StatTree) = {
    stat foreach {
      case t: Typed if t.getType == TUntyped => sys.error(s"Tree $t does not have a type!")
      case _                                 =>
    }
    code += stat
  }

  def putVarDecl(idName: String, initExpression: ExprTree): VariableID = {
    val decl = createVarDecl(idName, initExpression)
    code += decl
    decl.id
  }

  def createMethodCall(obj: ExprTree, classSymbol: ClassSymbol, methName: String, importMap: ImportMap, args: List[Type] = List()): NormalAccess = {
    val methodSymbol = classSymbol.lookupMethod(methName, args, importMap).get
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
    case _: TInt    => IntLit(1)
    case _: TChar   => IntLit(1)
    case _: TLong   => LongLit(1l)
    case _: TFloat  => FloatLit(1.0f)
    case _: TDouble => DoubleLit(1.0)
    case _          => ???
  }


  def getTypeTree(tpe: Type): TypeTree = (tpe match {
    case TUnit                => UnitType()
    case _: TChar             => CharType()
    case _: TBool             => BooleanType()
    case _: TInt              => IntType()
    case _: TLong             => LongType()
    case _: TFloat            => FloatType()
    case _: TDouble           => DoubleType()
    case TArray(t)            => ArrayType(getTypeTree(t))
    case TObject(classSymbol) => ClassID(classSymbol.name).setSymbol(classSymbol)
    case _                    => ???
  }).setType(tpe)

  def getCode = {
    val g = GeneratedExpr(code.toList).setPos(code.head).setType(TUnit)
    code.last match {
      case t: Typed => g.setType(t)
      case _        => g
    }
  }

  def setPos(pos: Positioned) = code.foreach(_.setPos(pos))

}




