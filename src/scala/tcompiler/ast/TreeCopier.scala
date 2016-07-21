package tcompiler.ast

import tcompiler.ast.Trees._
import tcompiler.imports.ImportMap

/**
  * Created by Tim Lindeberg on 5/22/2016.
  */

class TreeCopier {
  def CompilationUnit(t: Tree, pack: Package, classes: List[ClassDecl], importMap: ImportMap) =
    new CompilationUnit(pack, classes, importMap).copyAttrs(t)

  /*-------------------------------- Package and Import Trees --------------------------------*/

  def Package(t: Tree, adress: List[String]) =
    new Package(adress).copyAttrs(t)
  def RegularImport(t: Tree, adress: List[String]) =
    new RegularImport(adress).copyAttrs(t)
  def WildCardImport(t: Tree, adress: List[String]) =
    new WildCardImport(adress).copyAttrs(t)

  /*-------------------------------- Class Declaration Trees --------------------------------*/

  def ClassDecl(t: Tree, id: ClassID, parents: List[ClassID], fields: List[VarDecl], methods: List[FuncTree], isTrait: Boolean) =
    new ClassDecl(id, parents, fields, methods, isTrait).copyAttrs(t)

  /*-------------------------------- Modifier Trees --------------------------------*/

  def Public(t: Tree) =
    new Public().copyAttrs(t)
  def Private(t: Tree) =
    new Private().copyAttrs(t)
  def Protected(t: Tree) =
    new Protected().copyAttrs(t)
  def Static(t: Tree) =
    new Static().copyAttrs(t)
  def Implicit(t: Tree) =
    new Implicit().copyAttrs(t)
  def Final(t: Tree) =
    new Final().copyAttrs(t)

  /*-------------------------------- Function Declaration Trees --------------------------------*/

  def MethodDecl(t: Tree, retType: Option[TypeTree], id: MethodID, args: List[Formal], stat: Option[StatTree], modifiers: Set[Modifier]) =
    new MethodDecl(retType, id, args, stat, modifiers).copyAttrs(t)
  def ConstructorDecl(t: Tree, retType: Option[TypeTree], id: MethodID, args: List[Formal], stat: Option[StatTree], modifiers: Set[Modifier]) =
    new ConstructorDecl(retType, id, args, stat, modifiers).copyAttrs(t)
  def OperatorDecl(t: Tree, operatorType: OperatorTree, retType: Option[TypeTree], args: List[Formal], stat: Option[StatTree], modifiers: Set[Modifier]) =
    new OperatorDecl(operatorType, retType, args, stat, modifiers).copyAttrs(t)
  def Formal(t: Tree, tpe: TypeTree, id: VariableID) =
    new Formal(tpe, id).copyAttrs(t)


  /*-------------------------------- Type Trees --------------------------------*/

  def ArrayType(t: Tree, tpe: TypeTree) =
    new ArrayType(tpe).copyAttrs(t)
  def NullableType(t: Tree, tpe: TypeTree) =
    new NullableType(tpe).copyAttrs(t)
  def IntType(t: Tree) =
    new IntType().copyAttrs(t)
  def LongType(t: Tree) =
    new LongType().copyAttrs(t)
  def FloatType(t: Tree) =
    new FloatType().copyAttrs(t)
  def DoubleType(t: Tree) =
    new DoubleType().copyAttrs(t)
  def BooleanType(t: Tree) =
    new BooleanType().copyAttrs(t)
  def CharType(t: Tree) =
    new CharType().copyAttrs(t)
  def UnitType(t: Tree) =
    new UnitType().copyAttrs(t)


  /*-------------------------------- Statement Trees --------------------------------*/

  def VarDecl(t: Tree, tpe: Option[TypeTree], id: VariableID, init: Option[ExprTree], modifiers: Set[Modifier]) =
    new VarDecl(tpe, id, init, modifiers).copyAttrs(t)
  def Block(t: Tree, stats: List[StatTree]) =
    new Block(stats).copyAttrs(t)
  def If(t: Tree, expr: ExprTree, thn: StatTree, els: Option[StatTree]) =
    new If(expr, thn, els).copyAttrs(t)
  def While(t: Tree, expr: ExprTree, stat: StatTree) =
    new While(expr, stat).copyAttrs(t)
  def For(t: Tree, init: List[StatTree], condition: ExprTree, post: List[StatTree], stat: StatTree) =
    new For(init, condition, post, stat).copyAttrs(t)
  def Foreach(t: Tree, varDecl: VarDecl, container: ExprTree, stat: StatTree) =
    new Foreach(varDecl, container, stat).copyAttrs(t)
  def Error(t: Tree, expr: ExprTree) =
    new Error(expr).copyAttrs(t)
  def Return(t: Tree, expr: Option[ExprTree]) =
    new Return(expr).copyAttrs(t)
  def Break(t: Tree) =
    new Break().copyAttrs(t)
  def Continue(t: Tree) =
    new Continue().copyAttrs(t)
  def Print(t: Tree, expr: ExprTree) =
    new Print(expr).copyAttrs(t)
  def Println(t: Tree, expr: ExprTree) =
    new Println(expr).copyAttrs(t)

  /*-------------------------------- Binary Operator Trees --------------------------------*/


  def Plus(t: Tree, lhs: ExprTree, rhs: ExprTree) =
    new Plus(lhs, rhs).copyAttrs(t)
  def Minus(t: Tree, lhs: ExprTree, rhs: ExprTree) =
    new Minus(lhs, rhs).copyAttrs(t)
  def Times(t: Tree, lhs: ExprTree, rhs: ExprTree) =
    new Times(lhs, rhs).copyAttrs(t)
  def Div(t: Tree, lhs: ExprTree, rhs: ExprTree) =
    new Div(lhs, rhs).copyAttrs(t)
  def Modulo(t: Tree, lhs: ExprTree, rhs: ExprTree) =
    new Modulo(lhs, rhs).copyAttrs(t)

  def And(t: Tree, lhs: ExprTree, rhs: ExprTree) =
    new And(lhs, rhs).copyAttrs(t)
  def Or(t: Tree, lhs: ExprTree, rhs: ExprTree) =
    new Or(lhs, rhs).copyAttrs(t)
  def LogicAnd(t: Tree, lhs: ExprTree, rhs: ExprTree) =
    new LogicAnd(lhs, rhs).copyAttrs(t)
  def LogicOr(t: Tree, lhs: ExprTree, rhs: ExprTree) =
    new LogicOr(lhs, rhs).copyAttrs(t)
  def LogicXor(t: Tree, lhs: ExprTree, rhs: ExprTree) =
    new LogicXor(lhs, rhs).copyAttrs(t)

  def LeftShift(t: Tree, lhs: ExprTree, rhs: ExprTree) =
    new LeftShift(lhs, rhs).copyAttrs(t)
  def RightShift(t: Tree, lhs: ExprTree, rhs: ExprTree) =
    new RightShift(lhs, rhs).copyAttrs(t)

  def LessThan(t: Tree, lhs: ExprTree, rhs: ExprTree) =
    new LessThan(lhs, rhs).copyAttrs(t)
  def LessThanEquals(t: Tree, lhs: ExprTree, rhs: ExprTree) =
    new LessThanEquals(lhs, rhs).copyAttrs(t)
  def GreaterThan(t: Tree, lhs: ExprTree, rhs: ExprTree) =
    new GreaterThan(lhs, rhs).copyAttrs(t)
  def GreaterThanEquals(t: Tree, lhs: ExprTree, rhs: ExprTree) =
    new GreaterThanEquals(lhs, rhs).copyAttrs(t)

  def Equals(t: Tree, lhs: ExprTree, rhs: ExprTree) =
    new Equals(lhs, rhs).copyAttrs(t)
  def NotEquals(t: Tree, lhs: ExprTree, rhs: ExprTree) =
    new NotEquals(lhs, rhs).copyAttrs(t)

  /*-------------------------------- Unary Operator Trees --------------------------------*/

  def Not(t: Tree, expr: ExprTree) =
    new Not(expr).copyAttrs(t)
  def Hash(t: Tree, expr: ExprTree) =
    new Hash(expr).copyAttrs(t)
  def Negation(t: Tree, expr: ExprTree) =
    new Negation(expr).copyAttrs(t)
  def LogicNot(t: Tree, expr: ExprTree) =
    new LogicNot(expr).copyAttrs(t)

  def PreIncrement(t: Tree, expr: ExprTree) =
    new PreIncrement(expr).copyAttrs(t)
  def PreDecrement(t: Tree, expr: ExprTree) =
    new PreDecrement(expr).copyAttrs(t)
  def PostIncrement(t: Tree, expr: ExprTree) =
    new PostIncrement(expr).copyAttrs(t)
  def PostDecrement(t: Tree, expr: ExprTree) =
    new PostDecrement(expr).copyAttrs(t)

  /*-------------------------------- Array Operator Trees --------------------------------*/


  def ArrayAssign(t: Tree, arr: ExprTree, index: ExprTree, expr: ExprTree) =
    new ArrayAssign(arr, index, expr).copyAttrs(t)
  def ArrayRead(t: Tree, arr: ExprTree, index: ExprTree) =
    new ArrayRead(arr, index).copyAttrs(t)
  def ArraySlice(t: Tree, arr: ExprTree, start: Option[ExprTree], end: Option[ExprTree]) =
    new ArraySlice(arr, start, end).copyAttrs(t)

  /*-------------------------------- Literal and Identifer Trees --------------------------------*/

  def IntLit(t: Tree, value: Int) =
    new IntLit(value).copyAttrs(t)
  def LongLit(t: Tree, value: Long) =
    new LongLit(value).copyAttrs(t)
  def FloatLit(t: Tree, value: Float) =
    new FloatLit(value).copyAttrs(t)
  def DoubleLit(t: Tree, value: Double) =
    new DoubleLit(value).copyAttrs(t)
  def CharLit(t: Tree, value: Char) =
    new CharLit(value).copyAttrs(t)
  def StringLit(t: Tree, value: String) =
    new StringLit(value).copyAttrs(t)
  def ArrayLit(t: Tree, value: List[ExprTree]) =
    new ArrayLit(value).copyAttrs(t)
  def TrueLit(t: Tree) =
    new TrueLit().copyAttrs(t)
  def FalseLit(t: Tree) =
    new FalseLit().copyAttrs(t)
  def NullLit(t: Tree) =
    new NullLit().copyAttrs(t)
  def ClassID(t: Tree, name: String, templateTypes: List[TypeTree] = List()) =
    new ClassID(name, templateTypes).copyAttrs(t)
  def VariableID(t: Tree, name: String) =
    new VariableID(name).copyAttrs(t)
  def MethodID(t: Tree, name: String) =
    new MethodID(name).copyAttrs(t)

  /*-------------------------------- Access Trees --------------------------------*/

  def NormalAccess(t: Tree, obj:ExprTree, application:ExprTree) =
    new NormalAccess(obj, application).copyAttrs(t)
  def SafeAccess(t: Tree, obj:ExprTree, application:ExprTree) =
    new SafeAccess(obj, application).copyAttrs(t)

  /*-------------------------------- Expression Trees --------------------------------*/

  def Assign(t: Tree, to: ExprTree, expr: ExprTree) =
    new Assign(to, expr).copyAttrs(t)
  def MethodCall(t: Tree, meth: MethodID, args: List[ExprTree]) =
    new MethodCall(meth, args).copyAttrs(t)

  def This(t: Tree) =
    new This().copyAttrs(t)
  def Super(t: Tree, specifier: Option[ClassID]) =
    new Super(specifier).copyAttrs(t)
  def NewArray(t: Tree, tpe: TypeTree, sizes: List[ExprTree]) =
    new NewArray(tpe, sizes).copyAttrs(t)
  def New(t: Tree, tpe: TypeTree, args: List[ExprTree]) =
    new New(tpe, args).copyAttrs(t)
  def Ternary(t: Tree, condition: ExprTree, thn: ExprTree, els: ExprTree) =
    new Ternary(condition, thn, els).copyAttrs(t)
  def Elvis(t: Tree, nullableValue: ExprTree, ifNull:ExprTree) =
    new Elvis(nullableValue, ifNull).copyAttrs(t)
  def Is(t: Tree, expr: ExprTree, tpe: TypeTree) =
    new Is(expr, tpe).copyAttrs(t)
  def As(t: Tree, expr: ExprTree, tpe: TypeTree) =
    new As(expr, tpe).copyAttrs(t)
  def Empty(t: Tree) =
    new Empty().copyAttrs(t)
  def GeneratedExpr(t: Tree, stats: List[StatTree]) =
    new GeneratedExpr(stats).copyAttrs(t)
  def IfDup(t: Tree, expr: ExprTree) =
    new PutValue(expr).copyAttrs(t)
}

class LazyTreeCopier extends TreeCopier {
  override def CompilationUnit(tree: Tree, pack: Package, classes: List[ClassDecl], importMap: ImportMap) = tree match {
    case t@CompilationUnit(pack0, classes0, importMap0)
      if (pack eq pack0) && (classes eq classes0) && (importMap eq importMap0) => t
    case _ => super.CompilationUnit(tree, pack, classes, importMap)
  }
  override def Package(tree: Tree, adress: List[String]) = tree match {
    case t@Package(adress0)
      if (adress eq adress0) => t
    case _ => super.Package(tree, adress)
  }
  override def RegularImport(tree: Tree, adress: List[String]) = tree match {
    case t@RegularImport(adress0)
      if (adress eq adress0) => t
    case _ => super.RegularImport(tree, adress)
  }
  override def WildCardImport(tree: Tree, adress: List[String]) = tree match {
    case t@WildCardImport(adress0)
      if (adress eq adress0) => t
    case _ => super.WildCardImport(tree, adress)
  }
  override def ClassDecl(tree: Tree, id: ClassID, parents: List[ClassID], fields: List[VarDecl], methods: List[FuncTree], isTrait: Boolean) = tree match {
    case t@ClassDecl(id0, parents0, fields0, methods0, isTrait0)
      if (id eq id0) && (parents eq parents0) && (fields eq fields0) && (methods eq methods0) && (isTrait == isTrait0) => t
    case _ => super.ClassDecl(tree, id, parents, fields, methods, isTrait)
  }
  override def Public(tree: Tree) = tree match {
    case t@Public() => t
    case _ => super.Public(tree)
  }
  override def Private(tree: Tree) = tree match {
    case t@Private() => t
    case _ => super.Private(tree)
  }
  override def Protected(tree: Tree) = tree match {
    case t@Protected() => t
    case _ => super.Protected(tree)
  }
  override def Static(tree: Tree) = tree match {
    case t@Static() => t
    case _ => super.Static(tree)
  }
  override def Implicit(tree: Tree) = tree match {
    case t@Implicit() => t
    case _ => super.Implicit(tree)
  }
  override def Final(tree: Tree) = tree match {
    case t@Final() => t
    case _ => super.Final(tree)
  }
  override def MethodDecl(tree: Tree, retType: Option[TypeTree], id: MethodID, args: List[Formal], stat: Option[StatTree], modifiers: Set[Modifier]) = tree match {
    case t@MethodDecl(retType0, id0, args0, stat0, modifiers0)
      if (retType eq retType0) && (id eq id0) && (args eq args0) && (stat eq stat0) && (modifiers eq modifiers0) => t
    case _ => super.MethodDecl(tree, retType, id, args, stat, modifiers)
  }
  override def ConstructorDecl(tree: Tree, retType: Option[TypeTree], id: MethodID, args: List[Formal], stat: Option[StatTree], modifiers: Set[Modifier]) = tree match {
    case t@ConstructorDecl(retType0, id0, args0, stat0, modifiers0)
      if (retType eq retType0) && (id eq id0) && (args eq args0) && (stat eq stat0) && (modifiers eq modifiers0) => t
    case _ => super.ConstructorDecl(tree, retType, id, args, stat, modifiers)
  }
  override def OperatorDecl(tree: Tree, operatorType: OperatorTree, retType: Option[TypeTree], args: List[Formal], stat: Option[StatTree], modifiers: Set[Modifier]) = tree match {
    case t@OperatorDecl(operatorType0, retType0, args0, stat0, modifiers0)
      if (operatorType eq operatorType0) && (retType eq retType0) && (args eq args0) && (stat eq stat0) && (modifiers eq modifiers0) => t
    case _ => super.OperatorDecl(tree, operatorType, retType, args, stat, modifiers)
  }
  override def Formal(tree: Tree, tpe: TypeTree, id: VariableID) = tree match {
    case t@Formal(tpe0, id0)
      if (tpe eq tpe0) && (id eq id0) => t
    case _ => super.Formal(tree, tpe, id)
  }
  override def ArrayType(tree: Tree, tpe: TypeTree) = tree match {
    case t@ArrayType(tpe0)
      if (tpe eq tpe0) => t
    case _ => super.ArrayType(tree, tpe)
  }
  override def NullableType(tree: Tree, tpe: TypeTree) = tree match {
    case t@NullableType(tpe0)
      if (tpe eq tpe0) => t
    case _ => super.NullableType(tree, tpe)
  }
  override def IntType(tree: Tree) = tree match {
    case t@IntType() => t
    case _ => super.IntType(tree)
  }
  override def LongType(tree: Tree) = tree match {
    case t@LongType() => t
    case _ => super.LongType(tree)
  }
  override def FloatType(tree: Tree) = tree match {
    case t@FloatType() => t
    case _ => super.FloatType(tree)
  }
  override def DoubleType(tree: Tree) = tree match {
    case t@DoubleType() => t
    case _ => super.DoubleType(tree)
  }
  override def BooleanType(tree: Tree) = tree match {
    case t@BooleanType() => t
    case _ => super.BooleanType(tree)
  }
  override def CharType(tree: Tree) = tree match {
    case t@CharType() => t
    case _ => super.CharType(tree)
  }
  override def UnitType(tree: Tree) = tree match {
    case t@UnitType() => t
    case _ => super.UnitType(tree)
  }
  override def VarDecl(tree: Tree, tpe: Option[TypeTree], id: VariableID, init: Option[ExprTree], modifiers: Set[Modifier]) = tree match {
    case t@VarDecl(tpe0, id0, init0, modifiers0)
      if (tpe eq tpe0) && (id eq id0) && (init eq init0) && (modifiers eq modifiers0) => t
    case _ => super.VarDecl(tree, tpe, id, init, modifiers)
  }
  override def Block(tree: Tree, stats: List[StatTree]) = tree match {
    case t@Block(stats0)
      if (stats eq stats0) => t
    case _ => super.Block(tree, stats)
  }
  override def If(tree: Tree, expr: ExprTree, thn: StatTree, els: Option[StatTree]) = tree match {
    case t@If(expr0, thn0, els0)
      if (expr eq expr0) && (thn eq thn0) && (els eq els0) => t
    case _ => super.If(tree, expr, thn, els)
  }
  override def While(tree: Tree, expr: ExprTree, stat: StatTree) = tree match {
    case t@While(expr0, stat0)
      if (expr eq expr0) && (stat eq stat0) => t
    case _ => super.While(tree, expr, stat)
  }
  override def For(tree: Tree, init: List[StatTree], condition: ExprTree, post: List[StatTree], stat: StatTree) = tree match {
    case t@For(init0, condition0, post0, stat0)
      if (init eq init0) && (condition eq condition0) && (post eq post0) && (stat eq stat0) => t
    case _ => super.For(tree, init, condition, post, stat)
  }
  override def Foreach(tree: Tree, varDecl: VarDecl, container: ExprTree, stat: StatTree) = tree match {
    case t@Foreach(varDecl0, container0, stat0)
      if (varDecl eq varDecl0) && (container eq container0) && (stat eq stat0) => t
    case _ => super.Foreach(tree, varDecl, container, stat)
  }
  override def Error(tree: Tree, expr: ExprTree) = tree match {
    case t@Error(expr0)
      if (expr eq expr0) => t
    case _ => super.Error(tree, expr)
  }
  override def Return(tree: Tree, expr: Option[ExprTree]) = tree match {
    case t@Return(expr0)
      if (expr eq expr0) => t
    case _ => super.Return(tree, expr)
  }
  override def Break(tree: Tree) = tree match {
    case t@Break() => t
    case _ => super.Break(tree)
  }
  override def Continue(tree: Tree) = tree match {
    case t@Continue() => t
    case _ => super.Continue(tree)
  }
  override def Print(tree: Tree, expr: ExprTree) = tree match {
    case t@Print(expr0)
      if (expr eq expr0) => t
    case _ => super.Print(tree, expr)
  }
  override def Println(tree: Tree, expr: ExprTree) = tree match {
    case t@Println(expr0)
      if (expr eq expr0) => t
    case _ => super.Println(tree, expr)
  }
  override def Plus(tree: Tree, lhs: ExprTree, rhs: ExprTree) = tree match {
    case t@Plus(lhs0, rhs0)
      if (lhs eq lhs0) && (rhs eq rhs0) => t
    case _ => super.Plus(tree, lhs, rhs)
  }
  override def Minus(tree: Tree, lhs: ExprTree, rhs: ExprTree) = tree match {
    case t@Minus(lhs0, rhs0)
      if (lhs eq lhs0) && (rhs eq rhs0) => t
    case _ => super.Minus(tree, lhs, rhs)
  }
  override def Times(tree: Tree, lhs: ExprTree, rhs: ExprTree) = tree match {
    case t@Times(lhs0, rhs0)
      if (lhs eq lhs0) && (rhs eq rhs0) => t
    case _ => super.Times(tree, lhs, rhs)
  }
  override def Div(tree: Tree, lhs: ExprTree, rhs: ExprTree) = tree match {
    case t@Div(lhs0, rhs0)
      if (lhs eq lhs0) && (rhs eq rhs0) => t
    case _ => super.Div(tree, lhs, rhs)
  }
  override def Modulo(tree: Tree, lhs: ExprTree, rhs: ExprTree) = tree match {
    case t@Modulo(lhs0, rhs0)
      if (lhs eq lhs0) && (rhs eq rhs0) => t
    case _ => super.Modulo(tree, lhs, rhs)
  }
  override def And(tree: Tree, lhs: ExprTree, rhs: ExprTree) = tree match {
    case t@And(lhs0, rhs0)
      if (lhs eq lhs0) && (rhs eq rhs0) => t
    case _ => super.And(tree, lhs, rhs)
  }
  override def Or(tree: Tree, lhs: ExprTree, rhs: ExprTree) = tree match {
    case t@Or(lhs0, rhs0)
      if (lhs eq lhs0) && (rhs eq rhs0) => t
    case _ => super.Or(tree, lhs, rhs)
  }
  override def LogicAnd(tree: Tree, lhs: ExprTree, rhs: ExprTree) = tree match {
    case t@LogicAnd(lhs0, rhs0)
      if (lhs eq lhs0) && (rhs eq rhs0) => t
    case _ => super.LogicAnd(tree, lhs, rhs)
  }
  override def LogicOr(tree: Tree, lhs: ExprTree, rhs: ExprTree) = tree match {
    case t@LogicOr(lhs0, rhs0)
      if (lhs eq lhs0) && (rhs eq rhs0) => t
    case _ => super.LogicOr(tree, lhs, rhs)
  }
  override def LogicXor(tree: Tree, lhs: ExprTree, rhs: ExprTree) = tree match {
    case t@LogicXor(lhs0, rhs0)
      if (lhs eq lhs0) && (rhs eq rhs0) => t
    case _ => super.LogicXor(tree, lhs, rhs)
  }
  override def LeftShift(tree: Tree, lhs: ExprTree, rhs: ExprTree) = tree match {
    case t@LeftShift(lhs0, rhs0)
      if (lhs eq lhs0) && (rhs eq rhs0) => t
    case _ => super.LeftShift(tree, lhs, rhs)
  }
  override def RightShift(tree: Tree, lhs: ExprTree, rhs: ExprTree) = tree match {
    case t@RightShift(lhs0, rhs0)
      if (lhs eq lhs0) && (rhs eq rhs0) => t
    case _ => super.RightShift(tree, lhs, rhs)
  }
  override def LessThan(tree: Tree, lhs: ExprTree, rhs: ExprTree) = tree match {
    case t@LessThan(lhs0, rhs0)
      if (lhs eq lhs0) && (rhs eq rhs0) => t
    case _ => super.LessThan(tree, lhs, rhs)
  }
  override def LessThanEquals(tree: Tree, lhs: ExprTree, rhs: ExprTree) = tree match {
    case t@LessThanEquals(lhs0, rhs0)
      if (lhs eq lhs0) && (rhs eq rhs0) => t
    case _ => super.LessThanEquals(tree, lhs, rhs)
  }
  override def GreaterThan(tree: Tree, lhs: ExprTree, rhs: ExprTree) = tree match {
    case t@GreaterThan(lhs0, rhs0)
      if (lhs eq lhs0) && (rhs eq rhs0) => t
    case _ => super.GreaterThan(tree, lhs, rhs)
  }
  override def GreaterThanEquals(tree: Tree, lhs: ExprTree, rhs: ExprTree) = tree match {
    case t@GreaterThanEquals(lhs0, rhs0)
      if (lhs eq lhs0) && (rhs eq rhs0) => t
    case _ => super.GreaterThanEquals(tree, lhs, rhs)
  }
  override def Equals(tree: Tree, lhs: ExprTree, rhs: ExprTree) = tree match {
    case t@Equals(lhs0, rhs0)
      if (lhs eq lhs0) && (rhs eq rhs0) => t
    case _ => super.Equals(tree, lhs, rhs)
  }
  override def NotEquals(tree: Tree, lhs: ExprTree, rhs: ExprTree) = tree match {
    case t@NotEquals(lhs0, rhs0)
      if (lhs eq lhs0) && (rhs eq rhs0) => t
    case _ => super.NotEquals(tree, lhs, rhs)
  }
  override def Not(tree: Tree, expr: ExprTree) = tree match {
    case t@Not(expr0)
      if (expr eq expr0) => t
    case _ => super.Not(tree, expr)
  }
  override def Hash(tree: Tree, expr: ExprTree) = tree match {
    case t@Hash(expr0)
      if (expr eq expr0) => t
    case _ => super.Hash(tree, expr)
  }
  override def Negation(tree: Tree, expr: ExprTree) = tree match {
    case t@Negation(expr0)
      if (expr eq expr0) => t
    case _ => super.Negation(tree, expr)
  }
  override def LogicNot(tree: Tree, expr: ExprTree) = tree match {
    case t@LogicNot(expr0)
      if (expr eq expr0) => t
    case _ => super.LogicNot(tree, expr)
  }
  override def PreIncrement(tree: Tree, expr: ExprTree) = tree match {
    case t@PreIncrement(expr0)
      if (expr eq expr0) => t
    case _ => super.PreIncrement(tree, expr)
  }
  override def PreDecrement(tree: Tree, expr: ExprTree) = tree match {
    case t@PreDecrement(expr0)
      if (expr eq expr0) => t
    case _ => super.PreDecrement(tree, expr)
  }
  override def PostIncrement(tree: Tree, expr: ExprTree) = tree match {
    case t@PostIncrement(expr0)
      if (expr eq expr0) => t
    case _ => super.PostIncrement(tree, expr)
  }
  override def PostDecrement(tree: Tree, expr: ExprTree) = tree match {
    case t@PostDecrement(expr0)
      if (expr eq expr0) => t
    case _ => super.PostDecrement(tree, expr)
  }
  override def ArrayAssign(tree: Tree, arr: ExprTree, index: ExprTree, expr: ExprTree) = tree match {
    case t@ArrayAssign(arr0, index0, expr0)
      if (arr eq arr0) && (index eq index0) && (expr eq expr0) => t
    case _ => super.ArrayAssign(tree, arr, index, expr)
  }
  override def ArrayRead(tree: Tree, arr: ExprTree, index: ExprTree) = tree match {
    case t@ArrayRead(arr0, index0)
      if (arr eq arr0) && (index eq index0) => t
    case _ => super.ArrayRead(tree, arr, index)
  }
  override def ArraySlice(tree: Tree, arr: ExprTree, start: Option[ExprTree], end: Option[ExprTree]) = tree match {
    case t@ArraySlice(arr0, start0, end0)
      if (arr eq arr0) && (start eq start0) && (end eq end0) => t
    case _ => super.ArraySlice(tree, arr, start, end)
  }
  override def IntLit(tree: Tree, value: Int) = tree match {
    case t@IntLit(value0)
      if (value == value0) => t
    case _ => super.IntLit(tree, value)
  }
  override def LongLit(tree: Tree, value: Long) = tree match {
    case t@LongLit(value0)
      if (value == value0) => t
    case _ => super.LongLit(tree, value)
  }
  override def FloatLit(tree: Tree, value: Float) = tree match {
    case t@FloatLit(value0)
      if (value == value0) => t
    case _ => super.FloatLit(tree, value)
  }
  override def DoubleLit(tree: Tree, value: Double) = tree match {
    case t@DoubleLit(value0)
      if (value == value0) => t
    case _ => super.DoubleLit(tree, value)
  }
  override def CharLit(tree: Tree, value: Char) = tree match {
    case t@CharLit(value0)
      if (value == value0) => t
    case _ => super.CharLit(tree, value)
  }
  override def StringLit(tree: Tree, value: String) = tree match {
    case t@StringLit(value0)
      if (value eq value0) => t
    case _ => super.StringLit(tree, value)
  }
  override def ArrayLit(tree: Tree, value: List[ExprTree]) = tree match {
    case t@ArrayLit(value0)
      if (value eq value0) => t
    case _ => super.ArrayLit(tree, value)
  }
  override def TrueLit(tree: Tree) = tree match {
    case t@TrueLit() => t
    case _ => super.TrueLit(tree)
  }
  override def FalseLit(tree: Tree) = tree match {
    case t@FalseLit() => t
    case _ => super.FalseLit(tree)
  }
  override def NullLit(tree: Tree) = tree match {
    case t@NullLit() => t
    case _ => super.NullLit(tree)
  }
  override def ClassID(tree: Tree, name: String, templateTypes: List[TypeTree] = List()) = tree match {
    case t@ClassID(name0, templateTypes0)
      if (name eq name0) && (templateTypes eq templateTypes0) => t
    case _ => super.ClassID(tree, name, templateTypes)
  }
  override def VariableID(tree: Tree, name: String) = tree match {
    case t@VariableID(name0)
      if (name eq name0) => t
    case _ => super.VariableID(tree, name)
  }
  override def MethodID(tree: Tree, name: String) = tree match {
    case t@MethodID(name0)
      if (name eq name0) => t
    case _ => super.MethodID(tree, name)
  }
  override def NormalAccess(tree: Tree, obj:ExprTree, application:ExprTree) = tree match {
    case t@NormalAccess(obj0, application0)
      if (obj eq obj0) && (application eq application0) => t
    case _ => super.NormalAccess(tree, obj, application)
  }
  override def SafeAccess(tree: Tree, obj:ExprTree, application:ExprTree) = tree match {
    case t@SafeAccess(obj0, application0)
      if (obj eq obj0) && (application eq application0) => t
    case _ => super.SafeAccess(tree, obj, application)
  }
  override def Assign(tree: Tree, to: ExprTree, expr: ExprTree) = tree match {
    case t@Assign(to0, expr0)
      if (to eq to0) && (expr eq expr0) => t
    case _ => super.Assign(tree, to, expr)
  }
  override def MethodCall(tree: Tree, meth: MethodID, args: List[ExprTree]) = tree match {
    case t@MethodCall(meth0, args0)
      if (meth eq meth0) && (args eq args0) => t
    case _ => super.MethodCall(tree, meth, args)
  }
  override def This(tree: Tree) = tree match {
    case t@This() => t
    case _ => super.This(tree)
  }
  override def Super(tree: Tree, specifier: Option[ClassID]) = tree match {
    case t@Super(specifier0)
      if (specifier eq specifier0) => t
    case _ => super.Super(tree, specifier)
  }
  override def NewArray(tree: Tree, tpe: TypeTree, sizes: List[ExprTree]) = tree match {
    case t@NewArray(tpe0, sizes0)
      if (tpe eq tpe0) && (sizes eq sizes0) => t
    case _ => super.NewArray(tree, tpe, sizes)
  }
  override def New(tree: Tree, tpe: TypeTree, args: List[ExprTree]) = tree match {
    case t@New(tpe0, args0)
      if (tpe eq tpe0) && (args eq args0) => t
    case _ => super.New(tree, tpe, args)
  }
  override def Ternary(tree: Tree, condition: ExprTree, thn: ExprTree, els: ExprTree) = tree match {
    case t@Ternary(condition0, thn0, els0)
      if (condition eq condition0) && (thn eq thn0) && (els eq els0) => t
    case _ => super.Ternary(tree, condition, thn, els)
  }
  override def Elvis(tree: Tree, nullableValue: ExprTree, ifNull:ExprTree) = tree match {
    case t@Elvis(nullableValue0, ifNull0)
      if (nullableValue eq nullableValue0) && (ifNull eq ifNull0) => t
    case _ => super.Elvis(tree, nullableValue, ifNull)
  }
  override def Is(tree: Tree, expr: ExprTree, tpe: TypeTree) = tree match {
    case t@Is(expr0, tpe0)
      if (expr eq expr0) && (tpe eq tpe0) => t
    case _ => super.Is(tree, expr, tpe)
  }
  override def As(tree: Tree, expr: ExprTree, tpe: TypeTree) = tree match {
    case t@As(expr0, tpe0)
      if (expr eq expr0) && (tpe eq tpe0) => t
    case _ => super.As(tree, expr, tpe)
  }
  override def Empty(tree: Tree) = tree match {
    case t@Empty() => t
    case _ => super.Empty(tree)
  }
  override def GeneratedExpr(tree: Tree, stats: List[StatTree]) = tree match {
    case t@GeneratedExpr(stats0)
      if (stats eq stats0) => t
    case _ => super.GeneratedExpr(tree, stats)
  }
  override def IfDup(tree: Tree, expr: ExprTree) = tree match {
    case t@PutValue(expr0)
      if (expr eq expr0) => t
    case _ => super.IfDup(tree, expr)
  }
}


//object GenLazyCopier {
//
//
//  def main(args: Array[String]): Unit = {
//      var s =""
//      formatAllCode(s)
//  }
//
//
//  private def formatAllCode(s: String) = s.split("\n").foreach(formatCode)
//
//  private def formatCode(str: String): Unit = {
//    var s = str.trim
//    s = s.replace("t: Tree", "tree: Tree")
//    if(s.length == 0 || s.trim.startsWith("/*") || s.startsWith("new"))
//      return
//
//
//    val className = """def (.+?)\(""".r.findFirstMatchIn(s).get.group(1)
//    val p = """\((.+?)\)""".r.findFirstMatchIn(s).get.group(1)
//    val tpes = p.split(",").map(x => {
//      val split = x.split(":")
//      (split(0).trim, split(1).trim)
//    }).toMap
//    val params = p.split(",").map(_.split(":")(0).trim())
//
//    println(s"override $s tree match {")
//    print(s"   case t@$className(${params.drop(1).map(_ + "0").mkString(", ")})")
//    if(params.size > 1) {
//      println(s"\n   if ${params.drop(1).map(x => s"($x ${eqOrEquals(tpes, x)} ${x}0)").mkString(" && ")} => t")
//    }else{
//      println(" => t")
//    }
//    println(s"   case _ => super.$className(${params.mkString(", ")})")
//    println("}")
//  }
//
//  private def eqOrEquals(tpes: Map[String, String], id: String) = {
//    tpes(id) match {
//        case "Int" | "Char" | "Long" | "Double" | "Float" | "Boolean" => "=="
//        case _ => "eq"
//    }
//  }
//}


