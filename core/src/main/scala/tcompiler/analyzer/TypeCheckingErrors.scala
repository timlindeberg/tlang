package tcompiler.analyzer

import tcompiler.analyzer.Symbols.{MethodSymbol, OperatorSymbol, VariableSymbol}
import tcompiler.analyzer.Types.{TError, TObject, Type}
import tcompiler.ast.Trees.{PostDecrement, PostIncrement, PreDecrement, PreIncrement, _}
import tcompiler.error.{ErrorLevel, Errors}
import tcompiler.utils.Positioned

/**
  * Created by Tim Lindeberg on 5/13/2016.
  */
trait TypeCheckingErrors extends Errors {

  override val ErrorLetters = "T"

  def error(errorCode: Int, msg: String, pos: Positioned): Type = {
    if (!msg.contains(s"$TError"))
      report(errorCode, msg, ErrorLevel.Error, pos)
    TError
  }

  //---------------------------------------------------------------------------------------
  //  Error messages
  //---------------------------------------------------------------------------------------

  protected def ErrorWrongType(expected: Type, found: Type, pos: Positioned): Type = ErrorWrongType(err"$expected", err"$found", pos)

  protected def ErrorWrongType(expected: String, found: Type, pos: Positioned): Type = ErrorWrongType(expected, err"$found", pos)

  protected def ErrorWrongType(expected: Traversable[Type], found: Type, pos: Positioned): Type = {
    val s = makeExpectedString(expected)
    ErrorWrongType(s, found, pos)
  }

  protected def ErrorWrongType(expected: String, found: String, pos: Positioned): Type =
    error(0, err"Expected type: " + expected + err", found: " + found + err".", pos)

  protected def ErrorClassDoesntHaveMethod(className: String, methSignature: String, methName: String, alternatives: List[String], pos: Positioned): Type =
    error(1, err"Class $className does not contain a method $methSignature.${nameSuggestor(methName, alternatives)}", pos)

  protected def ErrorMethodOnWrongType(method: String, tpe: String, pos: Positioned): Type =
    error(2, err"Cannot call method $method on type $tpe.", pos)

  protected def ErrorClassDoesntHaveField(className: String, fieldName: String, alternatives: List[String], pos: Positioned): Type =
    error(3, err"Class $className does not contain a field $fieldName.${nameSuggestor(fieldName, alternatives)}", pos)

  protected def ErrorFieldOnWrongType(tpe: String, pos: Positioned): Type =
    error(4, err"Cannot access field on type $tpe.", pos)

  protected def ErrorNonStaticMethodAsStatic(methodName: String, pos: Positioned): Type =
    error(5, err"Trying to call method $methodName statically but the method is not declared as static.", pos)

  protected def ErrorNonStaticMethodFromStatic(methodName: String, pos: Positioned): Type =
    error(6, err"Cannot access non-static method $methodName from a static method.", pos)

  protected def ErrorNonStaticFieldAsStatic(fieldName: String, pos: Positioned): Type =
    error(7, err"Trying to access field $fieldName statically but the field is not declared as static.", pos)

  protected def ErrorNonStaticFieldFromStatic(fieldName: String, pos: Positioned): Type =
    error(8, err"Cannot access non-static field $fieldName from a static method.", pos)

  protected def ErrorConstructorPrivacy(methodSymbol: MethodSymbol, className: String, callingClass: String, pos: Positioned): Type = {
    val accessability = err"Cannot call " + accessabilityString(methodSymbol)
    error(9, accessability + err" constructor of $className from class $callingClass.", pos)
  }

  protected def ErrorMethodPrivacy(methodSymbol: MethodSymbol, className: String, callingClass: String, pos: Positioned): Type = {
    val accessability = err"Cannot call " + accessabilityString(methodSymbol)
    val methodName = methodSymbol.signature
    error(10, accessability + err" method $methodName defined in $className from class $callingClass.", pos)
  }

  protected def ErrorFieldPrivacy(varSymbol: VariableSymbol, className: String, callingClass: String, pos: Positioned): Type = {
    val accessability = err"Cannot access " + accessabilityString(varSymbol)
    val fieldName = varSymbol.name
    error(11, accessability + err" field $fieldName defined in $className from class $callingClass.", pos)
  }

  protected def ErrorOperatorPrivacy(opSymbol: OperatorSymbol, className: String, callingClass: String, pos: Positioned): Type = {
    val accessability = err"Cannot call " + accessabilityString(opSymbol)
    val operatorName = opSymbol.signature
    error(12, accessability + err" operator $operatorName defined in $className from class $callingClass.", pos)
  }

  protected def ErrorIndexingOperatorNotFound(expr: ArrayOperatorTree, args: List[Type], className: String, pos: Positioned): Type = {
    val operatorName = expr.operatorString(args, className)
    error(13, err"The class $className does not define an operator $operatorName.", pos)
  }

  protected def ErrorOverloadedOperatorNotFound(op: OperatorTree, args: List[Type], pos: Positioned): Type = {
    val classesString = overloadedOperatorClassesString(args)
    val operatorName = op.signature(args)
    error(13, classesString + err" define an operator $operatorName.", pos)
  }

  protected def ErrorOperatorWrongReturnType(op: String, expected: String, found: String, pos: Positioned): Type =
    error(14, err"Operator $op has wrong return type: expected $expected, found $found.", pos)

  protected def ErrorWrongReturnType(tpe: String, pos: Positioned): Type =
    error(15, err"Expected a return value of type $tpe.", pos)

  protected def ErrorDoesntHaveConstructor(className: String, methodSignature: String, pos: Positioned): Type =
    error(16, err"Class $className does not contain a constructor $methodSignature.", pos)

  // Missing 17

  protected def ErrorNoTypeNoInitalizer(name: String, pos: Positioned): Type =
    error(18, err"Variable $name declared with no type or initialization.", pos)

  protected def ErrorValueMustBeInitialized(name: String, pos: Positioned): Type =
    error(19, err"Value $name must be initialized.", pos)

  protected def ErrorNotOnNonNullable(pos: Positioned): Type =
    error(20, err"${"!"} operator can only be applied to ${"Bool"} and nullable types.", pos)

  protected def ErrorCantInferTypeRecursiveMethod(pos: Positioned): Type =
    error(21, err"Cannot infer type of recursive method.", pos)

  protected def ErrorInvalidIncrementDecrementExpr(expr: ExprTree, pos: Positioned): Type = {
    val msg = expr match {
      case _: PreIncrement | _: PostIncrement => err"Invalid increment expression."
      case _: PreDecrement | _: PostDecrement => err"Invalid decrement expression."
    }
    error(22, msg, pos)
  }

  // Missing 23

  protected def ErrorInstantiateTrait(treit: String, pos: Positioned): Type =
    error(24, err"Cannot instantiate trait $treit.", pos)

  protected def ErrorUnimplementedMethodFromTrait(clazz: String, method: String, treit: String, pos: Positioned): Type =
    error(25, err"Class $clazz does not implement method $method from trait $treit.", pos)

  protected def ErrorUnimplementedOperatorFromTrait(clazz: String, operator: String, tr: String, pos: Positioned): Type =
    error(26, err"Class $clazz does not implement operator $operator from trait $tr.", pos)

  protected def ErrorOverridingMethodDifferentReturnType(method: String, clazz: String, retType: String, parent: String, parentType: String, pos: Positioned): Type =
    error(27, err"Overriding method $method in class $clazz has return type $retType while the method in parent $parent has return type $parentType.", pos)

  protected def ErrorCantPrintUnitType(pos: Positioned): Type =
    error(28, err"Cannot print an expression of type ${"Unit"}.", pos)

  protected def ErrorNoSuperTypeHasMethod(clazz: String, method: String, pos: Positioned): Type =
    error(29, err"No super type of class $clazz implements a method $method.", pos)

  protected def ErrorNoSuperTypeHasField(clazz: String, field: String, pos: Positioned): Type =
    error(30, err"No super type of class $clazz has a field $field.", pos)

  // Missing 31

  protected def ErrorForeachNotIterable(tpe: Type, pos: Positioned): Type =
    error(32, err"Type $tpe is not iterable.", pos)

  protected def ErrorAssignNullToNonNullable(tpe: Type, pos: Positioned): Type =
    error(33, err"Cannot assign null to non nullable type $tpe.", pos)

  protected def ErrorSafeAccessOnNonNullable(tpe: Type, pos: Positioned): Type =
    error(34, err"Cannot use safe access on non nullable type $tpe.", pos)

  protected def ErrorExtractNullableNonNullable(tpe: Type, pos: Positioned): Type =
    error(35, err"Cannot use the nullable extraction operator on non nullable type $tpe.", pos)

  protected def ErrorElvisOperatorNonNullable(tpe: Type, pos: Positioned): Type =
    error(36, err"Cannot use the elvis operator on non-nullable type $tpe.", pos)

  protected def ErrorAssignValueToMethodCall(pos: Positioned): Type =
    error(37, err"Cannot assign a value to the result of a method call.", pos)

  protected def ErrorNonNullableEqualsNull(tpe: Type, pos: Positioned): Type =
    error(38, err"Cannot check if non nullable type $tpe is null.", pos)


  //---------------------------------------------------------------------------------------
  //  Warnings
  //---------------------------------------------------------------------------------------

  protected def WarningUnusedPrivateMethod(name: String, pos: Positioned): Unit =
    warning(0, err"Private method $name is never used.", pos)

  //---------------------------------------------------------------------------------------
  //  Private methods
  //---------------------------------------------------------------------------------------

  private def makeExpectedString(expected: Traversable[Type]): String = expected.size match {
    case 0 => ""
    case 1 => err"${expected.head}"
    case n => expected.take(n - 1).map(t => err"$t").mkString(", ") + " or " + expected.last + ""
  }

  private def accessabilityString(modifiable: Modifiable) = modifiable.accessability match {
    case Public()    => err"public"
    case Protected() => err"protected"
    case Private()   => err"private"
    case _           => ???
  }

  private def overloadedOperatorClassesString(args: List[Type]) =
    if (args.size != 2 || args(0) == args(1))
      err"The class ${args.head} does not"
    else if (!args(0).isInstanceOf[TObject])
      err"The class ${args(1)} does not"
    else if (!args(1).isInstanceOf[TObject])
      err"The class ${args(0)} does not"
    else
      err"None of the classes " + args.map(arg => err"$arg").mkString(err" or ")


}
