package tcompiler.ast

import tcompiler.ast.Trees._

/**
  * Created by Tim Lindeberg on 5/22/2016.
  */

abstract class TreeCopier {

  def Program(t: Tree, progPackage: Option[Package], imports: List[Import], classes: List[ClassDecl], importMap: Map[String, String]): Program

  /*-------------------------------- Package and Import Trees --------------------------------*/

  def Package(t: Tree, identifiers: List[Identifier]): Package
  def RegularImport(t: Tree, identifiers: List[Identifier]): RegularImport
  def WildCardImport(t: Tree, identifiers: List[Identifier]): WildCardImport
  def TemplateImport(t: Tree, identifiers: List[Identifier]): TemplateImport

  /*-------------------------------- Class Declaration Trees --------------------------------*/

  def ClassDecl(t: Tree, id: ClassIdentifier, parents: List[ClassIdentifier], fields: List[VarDecl], methods: List[FuncTree], isTrait: Boolean): ClassDecl

  /*-------------------------------- Modifier Trees --------------------------------*/

  def Public(t: Tree): Public
  def Private(t: Tree): Private
  def Protected(t: Tree): Protected
  def Static(t: Tree): Static
  def Implicit(t: Tree): Implicit
  def Final(t: Tree): Final

  /*-------------------------------- Function Declaration Trees --------------------------------*/

  def MethodDecl(t: Tree, retType: Option[TypeTree], id: Identifier, args: List[Formal], stat: Option[StatTree], modifiers: Set[Modifier]): MethodDecl
  def ConstructorDecl(t: Tree, retType: Option[TypeTree], id: Identifier, args: List[Formal], stat: Option[StatTree], modifiers: Set[Modifier]): ConstructorDecl
  def OperatorDecl(t: Tree, operatorType: OperatorTree, retType: Option[TypeTree], args: List[Formal], stat: Option[StatTree], modifiers: Set[Modifier], id: Identifier = new Identifier("")): OperatorDecl
  def Formal(t: Tree, tpe: TypeTree, id: Identifier): Formal

  /*-------------------------------- Type Trees --------------------------------*/

  def ArrayType(t: Tree, tpe: TypeTree): ArrayType
  def NullableType(t: Tree, tpe: TypeTree): NullableType
  def IntType(t: Tree): IntType
  def LongType(t: Tree): LongType
  def FloatType(t: Tree): FloatType
  def DoubleType(t: Tree): DoubleType
  def BooleanType(t: Tree): BooleanType
  def CharType(t: Tree): CharType
  def StringType(t: Tree): StringType
  def UnitType(t: Tree): UnitType

  /*-------------------------------- Statement Trees --------------------------------*/

  def VarDecl(t: Tree, tpe: Option[TypeTree], id: Identifier, init: Option[ExprTree], modifiers: Set[Modifier]): VarDecl
  def Block(t: Tree, stats: List[StatTree]): Block
  def If(t: Tree, expr: ExprTree, thn: StatTree, els: Option[StatTree]): If
  def While(t: Tree, expr: ExprTree, stat: StatTree): While
  def For(t: Tree, init: List[StatTree], condition: ExprTree, post: List[StatTree], stat: StatTree): For
  def Foreach(t: Tree, varDecl: VarDecl, container: ExprTree, stat: StatTree): Foreach
  def Error(t: Tree, expr: ExprTree): Error
  def Return(t: Tree, expr: Option[ExprTree]): Return
  def Break(t: Tree): Break
  def Continue(t: Tree): Continue
  def Print(t: Tree, expr: ExprTree): Print
  def Println(t: Tree, expr: ExprTree): Println

  /*-------------------------------- Binary Operator Trees --------------------------------*/


  def Plus(t: Tree, lhs: ExprTree, rhs: ExprTree): Plus
  def Minus(t: Tree, lhs: ExprTree, rhs: ExprTree): Minus
  def Times(t: Tree, lhs: ExprTree, rhs: ExprTree): Times
  def Div(t: Tree, lhs: ExprTree, rhs: ExprTree): Div
  def Modulo(t: Tree, lhs: ExprTree, rhs: ExprTree): Modulo

  def And(t: Tree, lhs: ExprTree, rhs: ExprTree): And
  def Or(t: Tree, lhs: ExprTree, rhs: ExprTree): Or
  def LogicAnd(t: Tree, lhs: ExprTree, rhs: ExprTree): LogicAnd
  def LogicOr(t: Tree, lhs: ExprTree, rhs: ExprTree): LogicOr
  def LogicXor(t: Tree, lhs: ExprTree, rhs: ExprTree): LogicXor

  def LeftShift(t: Tree, lhs: ExprTree, rhs: ExprTree): LeftShift
  def RightShift(t: Tree, lhs: ExprTree, rhs: ExprTree): RightShift

  def LessThan(t: Tree, lhs: ExprTree, rhs: ExprTree): LessThan
  def LessThanEquals(t: Tree, lhs: ExprTree, rhs: ExprTree): LessThanEquals
  def GreaterThan(t: Tree, lhs: ExprTree, rhs: ExprTree): GreaterThan
  def GreaterThanEquals(t: Tree, lhs: ExprTree, rhs: ExprTree): GreaterThanEquals

  def Equals(t: Tree, lhs: ExprTree, rhs: ExprTree): Equals
  def NotEquals(t: Tree, lhs: ExprTree, rhs: ExprTree): NotEquals

  /*-------------------------------- Unary Operator Trees --------------------------------*/

  def Not(t: Tree, expr: ExprTree): Not
  def Hash(t: Tree, expr: ExprTree): Hash
  def Negation(t: Tree, expr: ExprTree): Negation
  def LogicNot(t: Tree, expr: ExprTree): LogicNot

  def PreIncrement(t: Tree, expr: ExprTree): PreIncrement
  def PreDecrement(t: Tree, expr: ExprTree): PreDecrement
  def PostIncrement(t: Tree, expr: ExprTree): PostIncrement
  def PostDecrement(t: Tree, expr: ExprTree): PostDecrement

  /*-------------------------------- Array Operator Trees --------------------------------*/


  def ArrayAssign(t: Tree, arr: ExprTree, index: ExprTree, expr: ExprTree): ArrayAssign
  def ArrayRead(t: Tree, arr: ExprTree, index: ExprTree): ArrayRead
  def ArraySlice(t: Tree, arr: ExprTree, start: Option[ExprTree], end: Option[ExprTree]): ArraySlice

  /*-------------------------------- Literal and Identifer Trees --------------------------------*/

  def IntLit(t: Tree, value: Int): IntLit
  def LongLit(t: Tree, value: Long): LongLit
  def FloatLit(t: Tree, value: Float): FloatLit
  def DoubleLit(t: Tree, value: Double): DoubleLit
  def CharLit(t: Tree, value: Char): CharLit
  def StringLit(t: Tree, value: String): StringLit
  def ArrayLit(t: Tree, value: List[ExprTree]): ArrayLit
  def True(t: Tree): True
  def False(t: Tree): False
  def Null(t: Tree): Null
  def Identifier(t: Tree, value: String): Identifier
  def ClassIdentifier(t: Tree, value: String, templateTypes: List[TypeTree] = List()): ClassIdentifier

  /*-------------------------------- Expression Trees --------------------------------*/

  def Assign(t: Tree, id: Identifier, expr: ExprTree): Assign
  def FieldAssign(t: Tree, obj: ExprTree, id: Identifier, expr: ExprTree): FieldAssign
  def FieldRead(t: Tree, obj: ExprTree, id: Identifier): FieldRead
  def This(t: Tree): This
  def Super(t: Tree, specifier: Option[Identifier]): Super
  def NewArray(t: Tree, tpe: TypeTree, sizes: List[ExprTree]): NewArray
  def New(t: Tree, tpe: TypeTree, args: List[ExprTree]): New
  def Ternary(t: Tree, condition: ExprTree, thn: ExprTree, els: ExprTree): Ternary
  def Instance(t: Tree, expr: ExprTree, id: Identifier): Instance
  def As(t: Tree, expr: ExprTree, tpe: TypeTree): As
  def MethodCall(t: Tree, obj: ExprTree, meth: Identifier, args: List[ExprTree]): MethodCall

  def Empty(t: Tree): Empty

}

class StrictTreeCopier extends TreeCopier {
  override def Program(t: Tree, progPackage: Option[Package], imports: List[Import], classes: List[ClassDecl], importMap: Map[String, String]) =
    new Program(progPackage, imports, classes, importMap).copyAttrs(t)

  /*-------------------------------- Package and Import Trees --------------------------------*/

  override def Package(t: Tree, identifiers: List[Identifier]) =
    new Package(identifiers).copyAttrs(t)
  override def RegularImport(t: Tree, identifiers: List[Identifier]) =
    new RegularImport(identifiers).copyAttrs(t)
  override def WildCardImport(t: Tree, identifiers: List[Identifier]) =
    new WildCardImport(identifiers).copyAttrs(t)
  override def TemplateImport(t: Tree, identifiers: List[Identifier]) =
    new TemplateImport(identifiers).copyAttrs(t)


  /*-------------------------------- Class Declaration Trees --------------------------------*/

  override def ClassDecl(t: Tree, id: ClassIdentifier, parents: List[ClassIdentifier], fields: List[VarDecl], methods: List[FuncTree], isTrait: Boolean) =
    new ClassDecl(id, parents, fields, methods, isTrait).copyAttrs(t)

  /*-------------------------------- Modifier Trees --------------------------------*/

  override def Public(t: Tree) =
    new Public().copyAttrs(t)
  override def Private(t: Tree) =
    new Private().copyAttrs(t)
  override def Protected(t: Tree) =
    new Protected().copyAttrs(t)
  override def Static(t: Tree) =
    new Static().copyAttrs(t)
  override def Implicit(t: Tree) =
    new Implicit().copyAttrs(t)
  override def Final(t: Tree) =
    new Final().copyAttrs(t)

  /*-------------------------------- Function Declaration Trees --------------------------------*/

  override def MethodDecl(t: Tree, retType: Option[TypeTree], id: Identifier, args: List[Formal], stat: Option[StatTree], modifiers: Set[Modifier]) =
    new MethodDecl(retType, id, args, stat, modifiers).copyAttrs(t)
  override def ConstructorDecl(t: Tree, retType: Option[TypeTree], id: Identifier, args: List[Formal], stat: Option[StatTree], modifiers: Set[Modifier]) =
    new ConstructorDecl(retType, id, args, stat, modifiers).copyAttrs(t)
  override def OperatorDecl(t: Tree, operatorType: OperatorTree, retType: Option[TypeTree], args: List[Formal], stat: Option[StatTree], modifiers: Set[Modifier], id: Identifier) =
    new OperatorDecl(operatorType, retType, args, stat, modifiers, id).copyAttrs(t)
  override def Formal(t: Tree, tpe: TypeTree, id: Identifier) =
    new Formal(tpe, id).copyAttrs(t)


  /*-------------------------------- Type Trees --------------------------------*/

  override def ArrayType(t: Tree, tpe: TypeTree) =
    new ArrayType(tpe).copyAttrs(t)
  override def NullableType(t: Tree, tpe: TypeTree) =
    new NullableType(tpe).copyAttrs(t)
  override def IntType(t: Tree) =
    new IntType().copyAttrs(t)
  override def LongType(t: Tree) =
    new LongType().copyAttrs(t)
  override def FloatType(t: Tree) =
    new FloatType().copyAttrs(t)
  override def DoubleType(t: Tree) =
    new DoubleType().copyAttrs(t)
  override def BooleanType(t: Tree) =
    new BooleanType().copyAttrs(t)
  override def CharType(t: Tree) =
    new CharType().copyAttrs(t)
  override def StringType(t: Tree) =
    new StringType().copyAttrs(t)
  override def UnitType(t: Tree) =
    new UnitType().copyAttrs(t)


  /*-------------------------------- Statement Trees --------------------------------*/

  override def VarDecl(t: Tree, tpe: Option[TypeTree], id: Identifier, init: Option[ExprTree], modifiers: Set[Modifier]) =
    new VarDecl(tpe, id, init, modifiers).copyAttrs(t)
  override def Block(t: Tree, stats: List[StatTree]) =
    new Block(stats).copyAttrs(t)
  override def If(t: Tree, expr: ExprTree, thn: StatTree, els: Option[StatTree]) =
    new If(expr, thn, els).copyAttrs(t)
  override def While(t: Tree, expr: ExprTree, stat: StatTree) =
    new While(expr, stat).copyAttrs(t)
  override def For(t: Tree, init: List[StatTree], condition: ExprTree, post: List[StatTree], stat: StatTree) =
    new For(init, condition, post, stat).copyAttrs(t)
  override def Foreach(t: Tree, varDecl: VarDecl, container: ExprTree, stat: StatTree) =
    new Foreach(varDecl, container, stat).copyAttrs(t)
  override def Error(t: Tree, expr: ExprTree) =
    new Error(expr).copyAttrs(t)
  override def Return(t: Tree, expr: Option[ExprTree]) =
    new Return(expr).copyAttrs(t)
  override def Break(t: Tree) =
    new Break().copyAttrs(t)
  override def Continue(t: Tree) =
    new Continue().copyAttrs(t)
  override def Print(t: Tree, expr: ExprTree) =
    new Print(expr).copyAttrs(t)
  override def Println(t: Tree, expr: ExprTree) =
    new Println(expr).copyAttrs(t)

  /*-------------------------------- Binary Operator Trees --------------------------------*/


  override def Plus(t: Tree, lhs: ExprTree, rhs: ExprTree) =
    new Plus(lhs, rhs).copyAttrs(t)
  override def Minus(t: Tree, lhs: ExprTree, rhs: ExprTree) =
    new Minus(lhs, rhs).copyAttrs(t)
  override def Times(t: Tree, lhs: ExprTree, rhs: ExprTree) =
    new Times(lhs, rhs).copyAttrs(t)
  override def Div(t: Tree, lhs: ExprTree, rhs: ExprTree) =
    new Div(lhs, rhs).copyAttrs(t)
  override def Modulo(t: Tree, lhs: ExprTree, rhs: ExprTree) =
    new Modulo(lhs, rhs).copyAttrs(t)

  override def And(t: Tree, lhs: ExprTree, rhs: ExprTree) =
    new And(lhs, rhs).copyAttrs(t)
  override def Or(t: Tree, lhs: ExprTree, rhs: ExprTree) =
    new Or(lhs, rhs).copyAttrs(t)
  override def LogicAnd(t: Tree, lhs: ExprTree, rhs: ExprTree) =
    new LogicAnd(lhs, rhs).copyAttrs(t)
  override def LogicOr(t: Tree, lhs: ExprTree, rhs: ExprTree) =
    new LogicOr(lhs, rhs).copyAttrs(t)
  override def LogicXor(t: Tree, lhs: ExprTree, rhs: ExprTree) =
    new LogicXor(lhs, rhs).copyAttrs(t)

  override def LeftShift(t: Tree, lhs: ExprTree, rhs: ExprTree) =
    new LeftShift(lhs, rhs).copyAttrs(t)
  override def RightShift(t: Tree, lhs: ExprTree, rhs: ExprTree) =
    new RightShift(lhs, rhs).copyAttrs(t)

  override def LessThan(t: Tree, lhs: ExprTree, rhs: ExprTree) =
    new LessThan(lhs, rhs).copyAttrs(t)
  override def LessThanEquals(t: Tree, lhs: ExprTree, rhs: ExprTree) =
    new LessThanEquals(lhs, rhs).copyAttrs(t)
  override def GreaterThan(t: Tree, lhs: ExprTree, rhs: ExprTree) =
    new GreaterThan(lhs, rhs).copyAttrs(t)
  override def GreaterThanEquals(t: Tree, lhs: ExprTree, rhs: ExprTree) =
    new GreaterThanEquals(lhs, rhs).copyAttrs(t)

  override def Equals(t: Tree, lhs: ExprTree, rhs: ExprTree) =
    new Equals(lhs, rhs).copyAttrs(t)
  override def NotEquals(t: Tree, lhs: ExprTree, rhs: ExprTree) =
    new NotEquals(lhs, rhs).copyAttrs(t)

  /*-------------------------------- Unary Operator Trees --------------------------------*/

  override def Not(t: Tree, expr: ExprTree) =
    new Not(expr).copyAttrs(t)
  override def Hash(t: Tree, expr: ExprTree) =
    new Hash(expr).copyAttrs(t)
  override def Negation(t: Tree, expr: ExprTree) =
    new Negation(expr).copyAttrs(t)
  override def LogicNot(t: Tree, expr: ExprTree) =
    new LogicNot(expr).copyAttrs(t)

  override def PreIncrement(t: Tree, expr: ExprTree) =
    new PreIncrement(expr).copyAttrs(t)
  override def PreDecrement(t: Tree, expr: ExprTree) =
    new PreDecrement(expr).copyAttrs(t)
  override def PostIncrement(t: Tree, expr: ExprTree) =
    new PostIncrement(expr).copyAttrs(t)
  override def PostDecrement(t: Tree, expr: ExprTree) =
    new PostDecrement(expr).copyAttrs(t)

  /*-------------------------------- Array Operator Trees --------------------------------*/


  override def ArrayAssign(t: Tree, arr: ExprTree, index: ExprTree, expr: ExprTree) =
    new ArrayAssign(arr, index, expr).copyAttrs(t)
  override def ArrayRead(t: Tree, arr: ExprTree, index: ExprTree) =
    new ArrayRead(arr, index).copyAttrs(t)
  override def ArraySlice(t: Tree, arr: ExprTree, start: Option[ExprTree], end: Option[ExprTree]) =
    new ArraySlice(arr, start, end).copyAttrs(t)

  /*-------------------------------- Literal and Identifer Trees --------------------------------*/

  override def IntLit(t: Tree, value: Int) =
    new IntLit(value).copyAttrs(t)
  override def LongLit(t: Tree, value: Long) =
    new LongLit(value).copyAttrs(t)
  override def FloatLit(t: Tree, value: Float) =
    new FloatLit(value).copyAttrs(t)
  override def DoubleLit(t: Tree, value: Double) =
    new DoubleLit(value).copyAttrs(t)
  override def CharLit(t: Tree, value: Char) =
    new CharLit(value).copyAttrs(t)
  override def StringLit(t: Tree, value: String) =
    new StringLit(value).copyAttrs(t)
  override def ArrayLit(t: Tree, value: List[ExprTree]) =
    new ArrayLit(value).copyAttrs(t)
  override def True(t: Tree) =
    new True().copyAttrs(t)
  override def False(t: Tree) =
    new False().copyAttrs(t)
  override def Null(t: Tree) =
    new Null().copyAttrs(t)
  override def Identifier(t: Tree, value: String) =
    new Identifier(value).copyAttrs(t)
  override def ClassIdentifier(t: Tree, value: String, templateTypes: List[TypeTree] = List()) =
    new ClassIdentifier(value, templateTypes).copyAttrs(t)

  /*-------------------------------- Expression Trees --------------------------------*/

  override def Assign(t: Tree, id: Identifier, expr: ExprTree) =
    new Assign(id, expr).copyAttrs(t)
  override def FieldAssign(t: Tree, obj: ExprTree, id: Identifier, expr: ExprTree) =
    new FieldAssign(obj, id, expr).copyAttrs(t)
  override def FieldRead(t: Tree, obj: ExprTree, id: Identifier) =
    new FieldRead(obj, id).copyAttrs(t)
  override def This(t: Tree) =
    new This().copyAttrs(t)
  override def Super(t: Tree, specifier: Option[Identifier]) =
    new Super(specifier).copyAttrs(t)
  override def NewArray(t: Tree, tpe: TypeTree, sizes: List[ExprTree]) =
    new NewArray(tpe, sizes).copyAttrs(t)
  override def New(t: Tree, tpe: TypeTree, args: List[ExprTree]) =
    new New(tpe, args).copyAttrs(t)
  override def Ternary(t: Tree, condition: ExprTree, thn: ExprTree, els: ExprTree) =
    new Ternary(condition, thn, els).copyAttrs(t)
  override def Instance(t: Tree, expr: ExprTree, id: Identifier) =
    new Instance(expr, id).copyAttrs(t)
  override def As(t: Tree, expr: ExprTree, tpe: TypeTree) =
    new As(expr, tpe).copyAttrs(t)
  override def MethodCall(t: Tree, obj: ExprTree, meth: Identifier, args: List[ExprTree]) =
    new MethodCall(obj, meth, args).copyAttrs(t)
  override def Empty(t: Tree) =
    new Empty().copyAttrs(t)
}

class LazyTreeCopier extends TreeCopier {
  val strictCopier = new StrictTreeCopier

  override def Program(tree: Tree, progPackage: Option[Package], imports: List[Import], classes: List[ClassDecl], importMap: Map[String, String]) =
    tree match {
      case t@Program(progPackage0, imports0, classes0, importMap0)
        if progPackage0 == progPackage && imports0 == imports && classes0 == classes && importMap0 == importMap0 => t
      case _                                                                                                     => strictCopier.Program(tree, progPackage, imports, classes, importMap)
    }

  override def Package(tree: Tree, identifiers: List[Identifier]) = tree match {
    case t@Package(identifiers0)
      if identifiers == identifiers0 => t
    case _                           => strictCopier.Package(tree, identifiers)
  }
  override def RegularImport(tree: Tree, identifiers: List[Identifier]) = tree match {
    case t@RegularImport(identifiers0)
      if identifiers == identifiers0 => t
    case _                           => strictCopier.RegularImport(tree, identifiers)
  }
  override def WildCardImport(tree: Tree, identifiers: List[Identifier]) = tree match {
    case t@WildCardImport(identifiers0)
      if identifiers == identifiers0 => t
    case _                           => strictCopier.WildCardImport(tree, identifiers)
  }
  override def TemplateImport(tree: Tree, identifiers: List[Identifier]) = tree match {
    case t@TemplateImport(identifiers0)
      if identifiers == identifiers0 => t
    case _                           => strictCopier.TemplateImport(tree, identifiers)
  }
  override def ClassDecl(tree: Tree, id: ClassIdentifier, parents: List[ClassIdentifier], fields: List[VarDecl], methods: List[FuncTree], isTrait: Boolean) = tree match {
    case t@ClassDecl(id0, parents0, fields0, methods0, isTrait0)
      if id == id0 && parents == parents0 && fields == fields0 && methods == methods0 && isTrait == isTrait0 => t
    case _                                                                                                   => strictCopier.ClassDecl(tree, id, parents, fields, methods, isTrait)
  }
  override def Public(tree: Tree) = tree match {
    case t@Public() => t
    case _          => strictCopier.Public(tree)
  }
  override def Private(tree: Tree) = tree match {
    case t@Private() => t
    case _           => strictCopier.Private(tree)
  }
  override def Protected(tree: Tree) = tree match {
    case t@Protected() => t
    case _             => strictCopier.Protected(tree)
  }
  override def Static(tree: Tree) = tree match {
    case t@Static() => t
    case _          => strictCopier.Static(tree)
  }
  override def Implicit(tree: Tree) = tree match {
    case t@Implicit() => t
    case _            => strictCopier.Implicit(tree)
  }
  override def Final(tree: Tree) = tree match {
    case t@Final() => t
    case _         => strictCopier.Final(tree)
  }
  override def MethodDecl(tree: Tree, retType: Option[TypeTree], id: Identifier, args: List[Formal], stat: Option[StatTree], modifiers: Set[Modifier]) = tree match {
    case t@MethodDecl(retType0, id0, args0, stat0, modifiers0)
      if retType == retType0 && id == id0 && args == args0 && stat == stat0 && modifiers == modifiers0 => t
    case _                                                                                             => strictCopier.MethodDecl(tree, retType, id, args, stat, modifiers)
  }
  override def ConstructorDecl(tree: Tree, retType: Option[TypeTree], id: Identifier, args: List[Formal], stat: Option[StatTree], modifiers: Set[Modifier]) = tree match {
    case t@ConstructorDecl(retType0, id0, args0, stat0, modifiers0)
      if retType == retType0 && id == id0 && args == args0 && stat == stat0 && modifiers == modifiers0 => t
    case _                                                                                             => strictCopier.ConstructorDecl(tree, retType, id, args, stat, modifiers)
  }
  override def OperatorDecl(tree: Tree, operatorType: OperatorTree, retType: Option[TypeTree], args: List[Formal], stat: Option[StatTree], modifiers: Set[Modifier], id: Identifier) = tree match {
    case t@OperatorDecl(operatorType0, retType0, args0, stat0, modifiers0, id0)
      if operatorType == operatorType0 && retType == retType0 && args == args0 && stat == stat0 && modifiers == modifiers0 && id == id0 => t
    case _                                                                                                                              => strictCopier.OperatorDecl(tree, operatorType, retType, args, stat, modifiers, id)
  }
  override def Formal(tree: Tree, tpe: TypeTree, id: Identifier) = tree match {
    case t@Formal(tpe0, id0)
      if tpe == tpe0 && id == id0 => t
    case _                        => strictCopier.Formal(tree, tpe, id)
  }
  override def ArrayType(tree: Tree, tpe: TypeTree) = tree match {
    case t@ArrayType(tpe0)
      if tpe == tpe0 => t
    case _           => strictCopier.ArrayType(tree, tpe)
  }
  override def NullableType(tree: Tree, tpe: TypeTree) = tree match {
    case t@NullableType(tpe0)
      if tpe == tpe0 => t
    case _           => strictCopier.NullableType(tree, tpe)
  }
  override def IntType(tree: Tree) = tree match {
    case t@IntType() => t
    case _           => strictCopier.IntType(tree)
  }
  override def LongType(tree: Tree) = tree match {
    case t@LongType() => t
    case _            => strictCopier.LongType(tree)
  }
  override def FloatType(tree: Tree) = tree match {
    case t@FloatType() => t
    case _             => strictCopier.FloatType(tree)
  }
  override def DoubleType(tree: Tree) = tree match {
    case t@DoubleType() => t
    case _              => strictCopier.DoubleType(tree)
  }
  override def BooleanType(tree: Tree) = tree match {
    case t@BooleanType() => t
    case _               => strictCopier.BooleanType(tree)
  }
  override def CharType(tree: Tree) = tree match {
    case t@CharType() => t
    case _            => strictCopier.CharType(tree)
  }
  override def StringType(tree: Tree) = tree match {
    case t@StringType() => t
    case _              => strictCopier.StringType(tree)
  }
  override def UnitType(tree: Tree) = tree match {
    case t@UnitType() => t
    case _            => strictCopier.UnitType(tree)
  }
  override def VarDecl(tree: Tree, tpe: Option[TypeTree], id: Identifier, init: Option[ExprTree], modifiers: Set[Modifier]) = tree match {
    case t@VarDecl(tpe0, id0, init0, modifiers0)
      if tpe == tpe0 && id == id0 && init == init0 && modifiers == modifiers0 => t
    case _                                                                    => strictCopier.VarDecl(tree, tpe, id, init, modifiers)
  }
  override def Block(tree: Tree, stats: List[StatTree]) = tree match {
    case t@Block(stats0)
      if stats == stats0 => t
    case _               => strictCopier.Block(tree, stats)
  }
  override def If(tree: Tree, expr: ExprTree, thn: StatTree, els: Option[StatTree]) = tree match {
    case t@If(expr0, thn0, els0)
      if expr == expr0 && thn == thn0 && els == els0 => t
    case _                                           => strictCopier.If(tree, expr, thn, els)
  }
  override def While(tree: Tree, expr: ExprTree, stat: StatTree) = tree match {
    case t@While(expr0, stat0)
      if expr == expr0 && stat == stat0 => t
    case _                              => strictCopier.While(tree, expr, stat)
  }
  override def For(tree: Tree, init: List[StatTree], condition: ExprTree, post: List[StatTree], stat: StatTree) = tree match {
    case t@For(init0, condition0, post0, stat0)
      if init == init0 && condition == condition0 && post == post0 && stat == stat0 => t
    case _                                                                          => strictCopier.For(tree, init, condition, post, stat)
  }
  override def Foreach(tree: Tree, varDecl: VarDecl, container: ExprTree, stat: StatTree) = tree match {
    case t@Foreach(varDecl0, container0, stat0)
      if varDecl == varDecl0 && container == container0 && stat == stat0 => t
    case _                                                               => strictCopier.Foreach(tree, varDecl, container, stat)
  }
  override def Error(tree: Tree, expr: ExprTree) = tree match {
    case t@Error(expr0)
      if expr == expr0 => t
    case _             => strictCopier.Error(tree, expr)
  }
  override def Return(tree: Tree, expr: Option[ExprTree]) = tree match {
    case t@Return(expr0)
      if expr == expr0 => t
    case _             => strictCopier.Return(tree, expr)
  }
  override def Break(tree: Tree) = tree match {
    case t@Break() => t
    case _         => strictCopier.Break(tree)
  }
  override def Continue(tree: Tree) = tree match {
    case t@Continue() => t
    case _            => strictCopier.Continue(tree)
  }
  override def Print(tree: Tree, expr: ExprTree) = tree match {
    case t@Print(expr0)
      if expr == expr0 => t
    case _             => strictCopier.Print(tree, expr)
  }
  override def Println(tree: Tree, expr: ExprTree) = tree match {
    case t@Println(expr0)
      if expr == expr0 => t
    case _             => strictCopier.Println(tree, expr)
  }

  override def Plus(tree: Tree, lhs: ExprTree, rhs: ExprTree) = tree match {
    case t@Plus(lhs0, rhs0)
      if lhs == lhs0 && rhs == rhs0 => t
    case _                          => strictCopier.Plus(tree, lhs, rhs)
  }
  override def Minus(tree: Tree, lhs: ExprTree, rhs: ExprTree) = tree match {
    case t@Minus(lhs0, rhs0)
      if lhs == lhs0 && rhs == rhs0 => t
    case _                          => strictCopier.Minus(tree, lhs, rhs)
  }
  override def Times(tree: Tree, lhs: ExprTree, rhs: ExprTree) = tree match {
    case t@Times(lhs0, rhs0)
      if lhs == lhs0 && rhs == rhs0 => t
    case _                          => strictCopier.Times(tree, lhs, rhs)
  }
  override def Div(tree: Tree, lhs: ExprTree, rhs: ExprTree) = tree match {
    case t@Div(lhs0, rhs0)
      if lhs == lhs0 && rhs == rhs0 => t
    case _                          => strictCopier.Div(tree, lhs, rhs)
  }
  override def Modulo(tree: Tree, lhs: ExprTree, rhs: ExprTree) = tree match {
    case t@Modulo(lhs0, rhs0)
      if lhs == lhs0 && rhs == rhs0 => t
    case _                          => strictCopier.Modulo(tree, lhs, rhs)
  }

  override def And(tree: Tree, lhs: ExprTree, rhs: ExprTree) = tree match {
    case t@And(lhs0, rhs0)
      if lhs == lhs0 && rhs == rhs0 => t
    case _                          => strictCopier.And(tree, lhs, rhs)
  }
  override def Or(tree: Tree, lhs: ExprTree, rhs: ExprTree) = tree match {
    case t@Or(lhs0, rhs0)
      if lhs == lhs0 && rhs == rhs0 => t
    case _                          => strictCopier.Or(tree, lhs, rhs)
  }
  override def LogicAnd(tree: Tree, lhs: ExprTree, rhs: ExprTree) = tree match {
    case t@LogicAnd(lhs0, rhs0)
      if lhs == lhs0 && rhs == rhs0 => t
    case _                          => strictCopier.LogicAnd(tree, lhs, rhs)
  }
  override def LogicOr(tree: Tree, lhs: ExprTree, rhs: ExprTree) = tree match {
    case t@LogicOr(lhs0, rhs0)
      if lhs == lhs0 && rhs == rhs0 => t
    case _                          => strictCopier.LogicOr(tree, lhs, rhs)
  }
  override def LogicXor(tree: Tree, lhs: ExprTree, rhs: ExprTree) = tree match {
    case t@LogicXor(lhs0, rhs0)
      if lhs == lhs0 && rhs == rhs0 => t
    case _                          => strictCopier.LogicXor(tree, lhs, rhs)
  }

  override def LeftShift(tree: Tree, lhs: ExprTree, rhs: ExprTree) = tree match {
    case t@LeftShift(lhs0, rhs0)
      if lhs == lhs0 && rhs == rhs0 => t
    case _                          => strictCopier.LeftShift(tree, lhs, rhs)
  }
  override def RightShift(tree: Tree, lhs: ExprTree, rhs: ExprTree) = tree match {
    case t@RightShift(lhs0, rhs0)
      if lhs == lhs0 && rhs == rhs0 => t
    case _                          => strictCopier.RightShift(tree, lhs, rhs)
  }

  override def LessThan(tree: Tree, lhs: ExprTree, rhs: ExprTree) = tree match {
    case t@LessThan(lhs0, rhs0)
      if lhs == lhs0 && rhs == rhs0 => t
    case _                          => strictCopier.LessThan(tree, lhs, rhs)
  }
  override def LessThanEquals(tree: Tree, lhs: ExprTree, rhs: ExprTree) = tree match {
    case t@LessThanEquals(lhs0, rhs0)
      if lhs == lhs0 && rhs == rhs0 => t
    case _                          => strictCopier.LessThanEquals(tree, lhs, rhs)
  }
  override def GreaterThan(tree: Tree, lhs: ExprTree, rhs: ExprTree) = tree match {
    case t@GreaterThan(lhs0, rhs0)
      if lhs == lhs0 && rhs == rhs0 => t
    case _                          => strictCopier.GreaterThan(tree, lhs, rhs)
  }
  override def GreaterThanEquals(tree: Tree, lhs: ExprTree, rhs: ExprTree) = tree match {
    case t@GreaterThanEquals(lhs0, rhs0)
      if lhs == lhs0 && rhs == rhs0 => t
    case _                          => strictCopier.GreaterThanEquals(tree, lhs, rhs)
  }

  override def Equals(tree: Tree, lhs: ExprTree, rhs: ExprTree) = tree match {
    case t@Equals(lhs0, rhs0)
      if lhs == lhs0 && rhs == rhs0 => t
    case _                          => strictCopier.Equals(tree, lhs, rhs)
  }
  override def NotEquals(tree: Tree, lhs: ExprTree, rhs: ExprTree) = tree match {
    case t@NotEquals(lhs0, rhs0)
      if lhs == lhs0 && rhs == rhs0 => t
    case _                          => strictCopier.NotEquals(tree, lhs, rhs)
  }
  override def Not(tree: Tree, expr: ExprTree) = tree match {
    case t@Not(expr0)
      if expr == expr0 => t
    case _             => strictCopier.Not(tree, expr)
  }
  override def Hash(tree: Tree, expr: ExprTree) = tree match {
    case t@Hash(expr0)
      if expr == expr0 => t
    case _             => strictCopier.Hash(tree, expr)
  }
  override def Negation(tree: Tree, expr: ExprTree) = tree match {
    case t@Negation(expr0)
      if expr == expr0 => t
    case _             => strictCopier.Negation(tree, expr)
  }
  override def LogicNot(tree: Tree, expr: ExprTree) = tree match {
    case t@LogicNot(expr0)
      if expr == expr0 => t
    case _             => strictCopier.LogicNot(tree, expr)
  }
  override def PreIncrement(tree: Tree, expr: ExprTree) = tree match {
    case t@PreIncrement(expr0)
      if expr == expr0 => t
    case _             => strictCopier.PreIncrement(tree, expr)
  }
  override def PreDecrement(tree: Tree, expr: ExprTree) = tree match {
    case t@PreDecrement(expr0)
      if expr == expr0 => t
    case _             => strictCopier.PreDecrement(tree, expr)
  }
  override def PostIncrement(tree: Tree, expr: ExprTree) = tree match {
    case t@PostIncrement(expr0)
      if expr == expr0 => t
    case _             => strictCopier.PostIncrement(tree, expr)
  }
  override def PostDecrement(tree: Tree, expr: ExprTree) = tree match {
    case t@PostDecrement(expr0)
      if expr == expr0 => t
    case _             => strictCopier.PostDecrement(tree, expr)
  }
  override def ArrayAssign(tree: Tree, arr: ExprTree, index: ExprTree, expr: ExprTree) = tree match {
    case t@ArrayAssign(arr0, index0, expr0)
      if arr == arr0 && index == index0 && expr == expr0 => t
    case _                                               => strictCopier.ArrayAssign(tree, arr, index, expr)
  }
  override def ArrayRead(tree: Tree, arr: ExprTree, index: ExprTree) = tree match {
    case t@ArrayRead(arr0, index0)
      if arr == arr0 && index == index0 => t
    case _                              => strictCopier.ArrayRead(tree, arr, index)
  }
  override def ArraySlice(tree: Tree, arr: ExprTree, start: Option[ExprTree], end: Option[ExprTree]) = tree match {
    case t@ArraySlice(arr0, start0, end0)
      if arr == arr0 && start == start0 && end == end0 => t
    case _                                             => strictCopier.ArraySlice(tree, arr, start, end)
  }
  override def IntLit(tree: Tree, value: Int) = tree match {
    case t@IntLit(value0)
      if value == value0 => t
    case _               => strictCopier.IntLit(tree, value)
  }
  override def LongLit(tree: Tree, value: Long) = tree match {
    case t@LongLit(value0)
      if value == value0 => t
    case _               => strictCopier.LongLit(tree, value)
  }
  override def FloatLit(tree: Tree, value: Float) = tree match {
    case t@FloatLit(value0)
      if value == value0 => t
    case _               => strictCopier.FloatLit(tree, value)
  }
  override def DoubleLit(tree: Tree, value: Double) = tree match {
    case t@DoubleLit(value0)
      if value == value0 => t
    case _               => strictCopier.DoubleLit(tree, value)
  }
  override def CharLit(tree: Tree, value: Char) = tree match {
    case t@CharLit(value0)
      if value == value0 => t
    case _               => strictCopier.CharLit(tree, value)
  }
  override def StringLit(tree: Tree, value: String) = tree match {
    case t@StringLit(value0)
      if value == value0 => t
    case _               => strictCopier.StringLit(tree, value)
  }
  override def ArrayLit(tree: Tree, value: List[ExprTree]) = tree match {
    case t@ArrayLit(value0)
      if value == value0 => t
    case _               => strictCopier.ArrayLit(tree, value)
  }
  override def True(tree: Tree) = tree match {
    case t@True() => t
    case _        => strictCopier.True(tree)
  }
  override def False(tree: Tree) = tree match {
    case t@False() => t
    case _         => strictCopier.False(tree)
  }
  override def Null(tree: Tree) = tree match {
    case t@Null() => t
    case _        => strictCopier.Null(tree)
  }
  override def Identifier(tree: Tree, value: String) = tree match {
    case t@Identifier(value0)
      if value == value0 => t
    case _               => strictCopier.Identifier(tree, value)
  }
  override def ClassIdentifier(tree: Tree, value: String, templateTypes: List[TypeTree] = List()) = tree match {
    case t@ClassIdentifier(value0, templateTypes0)
      if value == value0 && templateTypes == templateTypes0 => t
    case _                                                  => strictCopier.ClassIdentifier(tree, value, templateTypes)
  }
  override def Assign(tree: Tree, id: Identifier, expr: ExprTree) = tree match {
    case t@Assign(id0, expr0)
      if id == id0 && expr == expr0 => t
    case _                          => strictCopier.Assign(tree, id, expr)
  }
  override def FieldAssign(tree: Tree, obj: ExprTree, id: Identifier, expr: ExprTree) = tree match {
    case t@FieldAssign(obj0, id0, expr0)
      if obj == obj0 && id == id0 && expr == expr0 => t
    case _                                         => strictCopier.FieldAssign(tree, obj, id, expr)
  }
  override def FieldRead(tree: Tree, obj: ExprTree, id: Identifier) = tree match {
    case t@FieldRead(obj0, id0)
      if obj == obj0 && id == id0 => t
    case _                        => strictCopier.FieldRead(tree, obj, id)
  }
  override def This(tree: Tree) = tree match {
    case t@This() => t
    case _        => strictCopier.This(tree)
  }
  override def Super(tree: Tree, specifier: Option[Identifier]) = tree match {
    case t@Super(specifier0)
      if specifier == specifier0 => t
    case _                       => strictCopier.Super(tree, specifier)
  }
  override def NewArray(tree: Tree, tpe: TypeTree, sizes: List[ExprTree]) = tree match {
    case t@NewArray(tpe0, sizes0)
      if tpe == tpe0 && sizes == sizes0 => t
    case _                              => strictCopier.NewArray(tree, tpe, sizes)
  }
  override def New(tree: Tree, tpe: TypeTree, args: List[ExprTree]) = tree match {
    case t@New(tpe0, args0)
      if tpe == tpe0 && args == args0 => t
    case _                            => strictCopier.New(tree, tpe, args)
  }
  override def Ternary(tree: Tree, condition: ExprTree, thn: ExprTree, els: ExprTree) = tree match {
    case t@Ternary(condition0, thn0, els0)
      if condition == condition0 && thn == thn0 && els == els0 => t
    case _                                                     => strictCopier.Ternary(tree, condition, thn, els)
  }
  override def Instance(tree: Tree, expr: ExprTree, id: Identifier) = tree match {
    case t@Instance(expr0, id0)
      if expr == expr0 && id == id0 => t
    case _                          => strictCopier.Instance(tree, expr, id)
  }
  override def As(tree: Tree, expr: ExprTree, tpe: TypeTree) = tree match {
    case t@As(expr0, tpe0)
      if expr == expr0 && tpe == tpe0 => t
    case _                            => strictCopier.As(tree, expr, tpe)
  }
  override def MethodCall(tree: Tree, obj: ExprTree, meth: Identifier, args: List[ExprTree]) = tree match {
    case t@MethodCall(obj0, meth0, args0)
      if obj == obj0 && meth == meth0 && args == args0 => t
    case _                                             => strictCopier.MethodCall(tree, obj, meth, args)
  }
  override def Empty(tree: Tree) = tree match {
    case t@Empty() => t
    case _         => strictCopier.Empty(tree)
  }

  /**
    * Used to generate the above code.
    *
    */
  private def formatCode(s: String): Unit = {
    if(s.length == 0 || s.trim.startsWith("/*"))
      return
    val className = """def (.+?)\(""".r.findFirstMatchIn(s).get.group(1)
    val p = """\((.+?)\)""".r.findFirstMatchIn(s).get.group(1)
    val params = p.split(",").map(_.split(":")(0).trim())

    println(s"$s tree match {")
    print(s"   case t@$className(${params.drop(1).map(_ + "0").mkString(", ")})")
    if(params.size > 1) {
      println(s"\n   if ${params.drop(1).map(x => s"$x == ${x}0").mkString(" && ")} => t")
    }else{
      println(" => t")
    }
    println(s"   case _ => strictCopier.$className(${params.mkString(", ")})")
    println("}")
  }

}