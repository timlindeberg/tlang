package tcompiler.ast

import tcompiler.ast.Trees._

import scala.collection.mutable

/**
  * Created by Tim Lindeberg on 5/22/2016.
  */

class TreeCopier {
  def Program(t: Tree, progPackage: Package, imports: List[Import], classes: List[ClassDecl], importMap: mutable.Map[String, String]) =
    new Program(progPackage, imports, classes, importMap).copyAttrs(t)

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
  def True(t: Tree) =
    new True().copyAttrs(t)
  def False(t: Tree) =
    new False().copyAttrs(t)
  def Null(t: Tree) =
    new Null().copyAttrs(t)
  def ClassIdentifier(t: Tree, name: String, templateTypes: List[TypeTree] = List()) =
    new ClassID(name, templateTypes).copyAttrs(t)
  def VarIdentifier(t: Tree, name: String) =
    new VariableID(name).copyAttrs(t)
  def MethodIdentifier(t: Tree, name: String) =
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
  def Instance(t: Tree, expr: ExprTree, id: ClassID) =
    new Is(expr, id).copyAttrs(t)
  def As(t: Tree, expr: ExprTree, tpe: TypeTree) =
    new As(expr, tpe).copyAttrs(t)
  def Empty(t: Tree) =
    new Empty().copyAttrs(t)
}

object GenLazyCopier {

  /*
  def main(args: Array[String]): Unit = {
    val s = "def Package(t: Tree, identifiers: List[Identifier]): Package\n  def RegularImport(t: Tree, identifiers: List[Identifier]): RegularImport\n  def WildCardImport(t: Tree, identifiers: List[Identifier]): WildCardImport\n  def TemplateImport(t: Tree, identifiers: List[Identifier]): TemplateImport\n\n  /*-------------------------------- Class Declaration Trees --------------------------------*/\n\n  def ClassDecl(t: Tree, id: ClassIdentifier, parents: List[ClassIdentifier], fields: List[VarDecl], methods: List[FuncTree], isTrait: Boolean): ClassDecl\n\n  /*-------------------------------- Modifier Trees --------------------------------*/\n\n  def Public(t: Tree): Public\n  def Private(t: Tree): Private\n  def Protected(t: Tree): Protected\n  def Static(t: Tree): Static\n  def Implicit(t: Tree): Implicit\n  def Final(t: Tree): Final\n\n  /*-------------------------------- Function Declaration Trees --------------------------------*/\n\n  def MethodDecl(t: Tree, retType: Option[TypeTree], id: Identifier, args: List[Formal], stat: Option[StatTree], modifiers: Set[Modifier]): MethodDecl\n  def ConstructorDecl(t: Tree, retType: Option[TypeTree], id: Identifier, args: List[Formal], stat: Option[StatTree], modifiers: Set[Modifier]): ConstructorDecl\n  def OperatorDecl(t: Tree, operatorType: OperatorTree, retType: Option[TypeTree], args: List[Formal], stat: Option[StatTree], modifiers: Set[Modifier], id: Identifier = new Identifier(\"\")): OperatorDecl\n  def Formal(t: Tree, tpe: TypeTree, id: Identifier): Formal\n\n  /*-------------------------------- Type Trees --------------------------------*/\n\n  def ArrayType(t: Tree, tpe: TypeTree): ArrayType\n  def NullableType(t: Tree, tpe: TypeTree): NullableType\n  def IntType(t: Tree): IntType\n  def LongType(t: Tree): LongType\n  def FloatType(t: Tree): FloatType\n  def DoubleType(t: Tree): DoubleType\n  def BooleanType(t: Tree): BooleanType\n  def CharType(t: Tree): CharType\n  def StringType(t: Tree): StringType\n  def UnitType(t: Tree): UnitType\n\n  /*-------------------------------- Statement Trees --------------------------------*/\n\n  def VarDecl(t: Tree, tpe: Option[TypeTree], id: Identifier, init: Option[ExprTree], modifiers: Set[Modifier]): VarDecl\n  def Block(t: Tree, stats: List[StatTree]): Block\n  def If(t: Tree, expr: ExprTree, thn: StatTree, els: Option[StatTree]): If\n  def While(t: Tree, expr: ExprTree, stat: StatTree): While\n  def For(t: Tree, init: List[StatTree], condition: ExprTree, post: List[StatTree], stat: StatTree): For\n  def Foreach(t: Tree, varDecl: VarDecl, container: ExprTree, stat: StatTree): Foreach\n  def Error(t: Tree, expr: ExprTree): Error\n  def Return(t: Tree, expr: Option[ExprTree]): Return\n  def Break(t: Tree): Break\n  def Continue(t: Tree): Continue\n  def Print(t: Tree, expr: ExprTree): Print\n  def Println(t: Tree, expr: ExprTree): Println\n\n  /*-------------------------------- Binary Operator Trees --------------------------------*/\n\n\n  def Plus(t: Tree, lhs: ExprTree, rhs: ExprTree): Plus\n  def Minus(t: Tree, lhs: ExprTree, rhs: ExprTree): Minus\n  def Times(t: Tree, lhs: ExprTree, rhs: ExprTree): Times\n  def Div(t: Tree, lhs: ExprTree, rhs: ExprTree): Div\n  def Modulo(t: Tree, lhs: ExprTree, rhs: ExprTree): Modulo\n\n  def And(t: Tree, lhs: ExprTree, rhs: ExprTree): And\n  def Or(t: Tree, lhs: ExprTree, rhs: ExprTree): Or\n  def LogicAnd(t: Tree, lhs: ExprTree, rhs: ExprTree): LogicAnd\n  def LogicOr(t: Tree, lhs: ExprTree, rhs: ExprTree): LogicOr\n  def LogicXor(t: Tree, lhs: ExprTree, rhs: ExprTree): LogicXor\n\n  def LeftShift(t: Tree, lhs: ExprTree, rhs: ExprTree): LeftShift\n  def RightShift(t: Tree, lhs: ExprTree, rhs: ExprTree): RightShift\n\n  def LessThan(t: Tree, lhs: ExprTree, rhs: ExprTree): LessThan\n  def LessThanEquals(t: Tree, lhs: ExprTree, rhs: ExprTree): LessThanEquals\n  def GreaterThan(t: Tree, lhs: ExprTree, rhs: ExprTree): GreaterThan\n  def GreaterThanEquals(t: Tree, lhs: ExprTree, rhs: ExprTree): GreaterThanEquals\n\n  def Equals(t: Tree, lhs: ExprTree, rhs: ExprTree): Equals\n  def NotEquals(t: Tree, lhs: ExprTree, rhs: ExprTree): NotEquals\n\n  /*-------------------------------- Unary Operator Trees --------------------------------*/\n\n  def Not(t: Tree, expr: ExprTree): Not\n  def Hash(t: Tree, expr: ExprTree): Hash\n  def Negation(t: Tree, expr: ExprTree): Negation\n  def LogicNot(t: Tree, expr: ExprTree): LogicNot\n\n  def PreIncrement(t: Tree, expr: ExprTree): PreIncrement\n  def PreDecrement(t: Tree, expr: ExprTree): PreDecrement\n  def PostIncrement(t: Tree, expr: ExprTree): PostIncrement\n  def PostDecrement(t: Tree, expr: ExprTree): PostDecrement\n\n  /*-------------------------------- Array Operator Trees --------------------------------*/\n\n\n  def ArrayAssign(t: Tree, arr: ExprTree, index: ExprTree, expr: ExprTree): ArrayAssign\n  def ArrayRead(t: Tree, arr: ExprTree, index: ExprTree): ArrayRead\n  def ArraySlice(t: Tree, arr: ExprTree, start: Option[ExprTree], end: Option[ExprTree]): ArraySlice\n\n  /*-------------------------------- Literal and Identifer Trees --------------------------------*/\n\n  def IntLit(t: Tree, value: Int): IntLit\n  def LongLit(t: Tree, value: Long): LongLit\n  def FloatLit(t: Tree, value: Float): FloatLit\n  def DoubleLit(t: Tree, value: Double): DoubleLit\n  def CharLit(t: Tree, value: Char): CharLit\n  def StringLit(t: Tree, value: String): StringLit\n  def ArrayLit(t: Tree, value: List[ExprTree]): ArrayLit\n  def True(t: Tree): True\n  def False(t: Tree): False\n  def Null(t: Tree): Null\n  def Identifier(t: Tree, value: String): Identifier\n  def ClassIdentifier(t: Tree, value: String, templateTypes: List[TypeTree] = List()): ClassIdentifier\n\n  /*-------------------------------- Expression Trees --------------------------------*/\n\n  def Assign(t: Tree, id: Identifier, expr: ExprTree): Assign\n  def FieldAssign(t: Tree, obj: ExprTree, id: Identifier, expr: ExprTree): FieldAssign\n  def FieldRead(t: Tree, obj: ExprTree, id: Identifier): FieldRead\n  def This(t: Tree): This\n  def Super(t: Tree, specifier: Option[Identifier]): Super\n  def NewArray(t: Tree, tpe: TypeTree, sizes: List[ExprTree]): NewArray\n  def New(t: Tree, tpe: TypeTree, args: List[ExprTree]): New\n  def Ternary(t: Tree, condition: ExprTree, thn: ExprTree, els: ExprTree): Ternary\n  def Instance(t: Tree, expr: ExprTree, id: Identifier): Instance\n  def As(t: Tree, expr: ExprTree, tpe: TypeTree): As\n  def MethodCall(t: Tree, obj: ExprTree, meth: Identifier, args: List[ExprTree]): MethodCall\n\n  def Empty(t: Tree): Empty"
    formatAllCode(s)
  }
  */

  private def formatAllCode(s: String) = s.split("\n").foreach(formatCode)

  /**
    * Used to generate the above code.
    *
    */
  private def formatCode(str: String): Unit = {
    var s = str.trim
    s = s.replace("t: Tree", "tree: Tree")
    if(s.length == 0 || s.trim.startsWith("/*"))
      return


    val className = """def (.+?)\(""".r.findFirstMatchIn(s).get.group(1)
    val p = """\((.+?)\)""".r.findFirstMatchIn(s).get.group(1)
    val tpes = p.split(",").map(x => {
      val split = x.split(":")
      (split(0).trim, split(1).trim)
    }).toMap
    val params = p.split(",").map(_.split(":")(0).trim())

    println(s"override $s = tree match {")
    print(s"   case t@$className(${params.drop(1).map(_ + "0").mkString(", ")})")
    if(params.size > 1) {
      println(s"\n   if ${params.drop(1).map(x => s"($x ${eqOrEquals(tpes, x)} ${x}0)").mkString(" && ")} => t")
    }else{
      println(" => t")
    }
    println(s"   case _ => strictCopier.$className(${params.mkString(", ")})")
    println("}")
  }

  private def eqOrEquals(tpes: Map[String, String], id: String) = {
    tpes(id) match {
        case "Int" | "Char" | "Long" | "Double" | "Float" | "Boolean" => "=="
        case _ => "eq"
    }
  }

}