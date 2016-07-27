package tcompiler.analyzer

import tcompiler.analyzer.Symbols.{MethodSymbol, OperatorSymbol, VariableSymbol}
import tcompiler.analyzer.Types.{TError, TObject, Type}
import tcompiler.ast.Trees.{PostDecrement, PostIncrement, PreDecrement, PreIncrement, _}
import tcompiler.utils.{Errors, Positioned}

/**
  * Created by Tim Lindeberg on 5/13/2016.
  */
trait TypeCheckingErrors extends Errors {

  override val ErrorPrefix = "T"

  def error(errorCode: Int, msg: String, pos: Positioned): Type = {
    if (!msg.contains(s"'$TError'"))
      ctx.reporter.error(ErrorPrefix, errorCode, msg, pos, importMap)
    TError
  }

  //---------------------------------------------------------------------------------------
  //  Error messages
  //---------------------------------------------------------------------------------------

  protected def ErrorWrongType(expected: Type, found: Type, pos: Positioned): Type = ErrorWrongType(s"'$expected'", s"'$found'", pos)
  protected def ErrorWrongType(expected: Type, found: String, pos: Positioned): Type = ErrorWrongType(s"'$expected'", found, pos)
  protected def ErrorWrongType(expected: String, found: Type, pos: Positioned): Type = ErrorWrongType(expected, s"'$found'", pos)
  protected def ErrorWrongType(expected: Traversable[Type], found: Type, pos: Positioned): Type = {
    val s = makeExpectedString(expected)
    ErrorWrongType(s, found, pos)
  }
  protected def ErrorWrongType(expected: String, found: String, pos: Positioned): Type =
    error(0, s"Expected type: $expected, found: $found.", pos)

  protected def ErrorClassDoesntHaveMethod(className: String, methodSignature: String, pos: Positioned) =
    error(1, s"Class '$className' does not contain a method '$methodSignature'.", pos)

  protected def ErrorMethodOnWrongType(method: String, tpe: String, pos: Positioned) =
    error(2, s"Cannot call method '$method' on type '$tpe'.", pos)

  protected def ErrorClassDoesntHaveField(className: String, fieldName: String, pos: Positioned) =
    error(3, s"Class '$className' does not contain a field '$fieldName'.", pos)

  protected def ErrorFieldOnWrongType(tpe: String, pos: Positioned) =
    error(4, s"Cannot access field on type '$tpe'.", pos)

  protected def ErrorNonStaticMethodAsStatic(methodName: String, pos: Positioned) =
    error(5, s"Trying to call method '$methodName' statically but the method is not declared as static.", pos)

  protected def ErrorNonStaticMethodFromStatic(methodName: String, pos: Positioned) =
    error(6, s"Cannot access non-static method '$methodName' from a static method.", pos)

  protected def ErrorNonStaticFieldAsStatic(fieldName: String, pos: Positioned) =
    error(7, s"Trying to access field '$fieldName' statically but the field is not declared as static.", pos)

  protected def ErrorNonStaticFieldFromStatic(fieldName: String, pos: Positioned) =
    error(8, s"Cannot access non-static field '$fieldName' from a static method.", pos)

  protected def ErrorConstructorPrivacy(methodSymbol: MethodSymbol, className: String, callingClass: String, pos: Positioned) = {
    val accessability = accessabilityString(methodSymbol)
    error(9, s"Cannot call $accessability constructor of '$className' from class '$callingClass'.", pos)
  }

  protected def ErrorMethodPrivacy(methodSymbol: MethodSymbol, className: String, callingClass: String, pos: Positioned) = {
    val accessability = accessabilityString(methodSymbol)
    val methodName = methodSymbol.signature
    error(10, s"Cannot call $accessability method '$methodName' defined in '$className' from class '$callingClass'.", pos)
  }

  protected def ErrorFieldPrivacy(varSymbol: VariableSymbol, className: String, callingClass: String, pos: Positioned) = {
    val accessability = accessabilityString(varSymbol)
    val fieldName = varSymbol.name
    error(11, s"Cannot access $accessability field '$fieldName' defined in '$className' from class '$callingClass'.", pos)
  }

  protected def ErrorOperatorPrivacy(opSymbol: OperatorSymbol, className: String, callingClass: String, pos: Positioned) = {
    val accessability = accessabilityString(opSymbol)
    val operatorName = opSymbol.operatorString
    error(12, s"Cannot call $accessability operator '$operatorName' defined in '$className' from class '$callingClass'.", pos)
  }

  protected def ErrorIndexingOperatorNotFound(expr: ArrayOperatorTree, args: List[Type], className: String, pos: Positioned) = {
    val operatorName = expr.operatorString(args, className)
    error(13, s"The class '$className' does not contain an operator '$operatorName'.", pos)
  }

  protected def ErrorOverloadedOperatorNotFound(op: OperatorTree, args: List[Type], pos: Positioned): Type = {
    val classesString = overloadedOperatorClassesString(args)
    val operatorName = op.operatorString(args)
    error(13, s"$classesString contain an operator '$operatorName'.", pos)
  }

  protected def ErrorOperatorWrongReturnType(op: String, expected: String, found: String, pos: Positioned) = {
    error(14, s"Operator '$op' has wrong return type: expected '$expected', found '$found'.", pos)
  }

  protected def ErrorWrongReturnType(tpe: String, pos: Positioned) =
    error(15, s"Expected a return value of type '$tpe'.", pos)

  protected def ErrorDoesntHaveConstructor(className: String, methodSignature: String, pos: Positioned) =
    error(16, s"Class '$className' does not contain a constructor '$methodSignature'.", pos)

  protected def ErrorNewPrimitive(tpe: String, args: List[Type], pos: Positioned) =
    error(17, s"Cannot construct primitive '$tpe' with argument(s) '(${args.mkString(", ")})'.", pos)

  protected def ErrorNoTypeNoInitalizer(name: String, pos: Positioned) =
    error(18, s"Variable '$name' declared with no type or initialization.", pos)

  protected def ErrorMultipleReturnTypes(returnStatements: List[(Return, Type)]) = {
    val typeList = returnStatements.map { case (stat, tpe) => s"Line ${stat.line} -> '$tpe'" }.mkString(", ")
    error(19, s"Method contains return statements of multiple types: $typeList.", returnStatements.head._1)
  }

  protected def ErrorMultipleArrayLitTypes(typeList: String, pos: Positioned) =
    error(20, s"Array literal contains multiple types: $typeList", pos)

  protected def ErrorCantInferTypeRecursiveMethod(pos: Positioned) =
    error(21, s"Cannot infer type of recursive method.", pos)

  protected def ErrorInvalidIncrementDecrementExpr(expr: ExprTree, pos: Positioned) =
    error(22, s"Invalid ${incrementOrDecrement(expr)} expression.", pos)

  protected def ErrorOperatorDoesNotExist(expr: OperatorTree, args: (Type, Type), pos: Positioned): Type = {
    val operator = expr.operatorString(List(args._1, args._2))
    error(23, s"Operator '$operator' does not exist.", pos)
  }

  protected def ErrorInstantiateTrait(tr: String, pos: Positioned) =
    error(24, s"Cannot instantiate trait '$tr'.", pos)

  protected def ErrorUnimplementedMethodFromTrait(clazz: String, method: String, tr: String, pos: Positioned) =
    error(25, s"Class '$clazz' does not implement method '$method' from trait '$tr'.", pos)

  protected def ErrorUnimplementedOperatorFromTrait(clazz: String, operator: String, tr: String, pos: Positioned) =
    error(26, s"Class '$clazz' does not implement operator '$operator' from trait '$tr'.", pos)

  protected def ErrorOverridingMethodDifferentReturnType(method: String, clazz: String, retType: String, parent: String, parentType: String, pos: Positioned) =
    error(27, s"Overriding method '$method' in class '$clazz' has return type '$retType' while the method in parent '$parent' has return type '$parentType'.", pos)

  protected def ErrorCantPrintUnitType(pos: Positioned) =
    error(28, s"Cannot print an expression of type 'Unit'.", pos)

  protected def ErrorNoSuperTypeHasMethod(clazz: String, method: String, pos: Positioned) =
    error(29, s"No super type of class '$clazz' implements a method '$method'.", pos)

  protected def ErrorNoSuperTypeHasField(clazz: String, field: String, pos: Positioned) =
    error(30, s"No super type of class '$clazz' has a field '$field'.", pos)

  protected def ErrorReassignmentToVal(value: String, pos: Positioned) =
    error(31, s"Cannot reassign value '$value'.", pos)

  protected def ErrorForeachNotIterable(tpe: Type, pos: Positioned) =
    error(32, s"Type '$tpe' does not implement the 'Iterable' trait.", pos)

  protected def ErrorAssignNullToNonNullable(tpe: Type, pos: Positioned) =
    error(33, s"Cannot assign 'null' to non nullable type '$tpe'.", pos)

  protected def ErrorSafeAccessOnNonNullable(tpe: Type, pos: Positioned) =
    error(34, s"Cannot use safe access on non nullable type '$tpe'.", pos)

  protected def ErrorExtractNullableNonNullable(tpe: Type, pos: Positioned) =
    error(35, s"Cannot use the nullable extraction operator on non nullable type '$tpe'.", pos)

  protected def ErrorElvisOperatorNonNullable(tpe: Type, pos: Positioned) =
    error(36, s"Cannot use the elvis operator on non nullable type '$tpe'.", pos)

  protected def ErrorAssignValueToMethodCall(pos: Positioned) =
    error(37, s"Cannot assign a value to the result of a method call.", pos)

  protected def ErrorNonNullableEqualsNull(tpe: Type, pos: Positioned) =
    error(37, s"Cannot check if non nullable type '$tpe' is 'null'.", pos)



  //---------------------------------------------------------------------------------------
  //  Warnings
  //---------------------------------------------------------------------------------------

  protected def WarningUnusedPrivateField(name: String, pos: Positioned) =
    warning(0, s"protected method '$name' is never used.", pos)

  //---------------------------------------------------------------------------------------
  //  Private methods
  //---------------------------------------------------------------------------------------

  private def makeExpectedString(expected: Traversable[Type]): String = expected.size match {
    case 0 => ""
    case 1 => s"'${expected.head}'"
    case n => expected.take(n - 1).map(t => s"'$t'").mkString(", ") + " or '" + expected.last + "'"
  }

  private def accessabilityString(modifiable: Modifiable) = modifiable.accessability match {
    case Public()    => "public"
    case Protected() => "protected"
    case Private()   => "private"
    case _           => ???
  }

  private def overloadedOperatorClassesString(args: List[Type]) =
    if (args.size != 2 || args(0) == args(1)) {
      "The class \'" + args.head + "\' does not"
    } else {
      if (!args(0).isInstanceOf[TObject]) {
        "The class \'" + args(1) + "\' does not"
      } else if (!args(1).isInstanceOf[TObject]) {
        "The class \'" + args(0) + "\' does not"
      } else {
        "None of the classes " + args.map("'" + _.toString + "'").mkString(" or ")
      }
    }

  private def incrementOrDecrement(expr: ExprTree) =
    expr match {
      case _: PreIncrement | _: PostIncrement => "increment"
      case _: PreDecrement | _: PostDecrement => "decrement"
    }

}
