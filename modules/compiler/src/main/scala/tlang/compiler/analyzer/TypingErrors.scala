package tlang
package compiler
package analyzer

import tlang.compiler.analyzer.Symbols.{ClassSymbol, FieldSymbol, MethodSymbol, Symbol}
import tlang.compiler.analyzer.Types.{TError, TObject, TUnit, Type}
import tlang.compiler.ast.Trees._
import tlang.compiler.messages.{CompilerMessage, ErrorHandling, ErrorMessage, WarningMessage}
import tlang.utils.Positioned

trait TypingErrors extends ErrorHandling {


  def report(error: ErrorMessage): Type = {
    reporter.report(error)
    TError
  }

  //---------------------------------------------------------------------------------------
  //  Error messages
  //---------------------------------------------------------------------------------------

  import errorStringContext._


  private def containsErrorType(product: Product): Boolean =
    product.productIterator.exists {
      case s: String                                                             => s == CompilerMessage.ErrorName
      case t: Type                                                               => t == TError
      case list: Traversable[_] if list.nonEmpty && list.head.isInstanceOf[Type] =>
        list.asInstanceOf[Traversable[Type]].exists(_ == TError)
      case _                                                                     => false
    }

  private val ErrorLetters = "T"
  abstract class TypeCheckingError(code: Int, pos: Positioned) extends ErrorMessage(ErrorLetters, code, pos) {
    override def isValid: Boolean = !containsErrorType(this)
  }

  abstract class TypeCheckingWarning(code: Int, pos: Positioned) extends WarningMessage(ErrorLetters, code, pos) {
    override def isValid: Boolean = !containsErrorType(this)
  }

  object WrongType {
    def apply(expected: Type, found: Type, pos: Positioned): WrongType = WrongType(err"$expected", err"$found", pos)
    def apply(expected: String, found: Type, pos: Positioned): WrongType = WrongType(expected, err"$found", pos)
    def apply(expected: Traversable[Type], found: Type, pos: Positioned): WrongType = WrongType(makeExpectedString(expected), found, pos)

    private def makeExpectedString(expected: Traversable[Type]): String = expected.size match {
      case 0 => ""
      case 1 => err"${ expected.head }"
      case n => expected.take(n - 1).map(t => err"$t").mkString(", ") + " or " + expected.last + ""
    }

  }
  case class WrongType(expected: String, found: String, override val pos: Positioned)
    extends TypeCheckingError(0, pos) {
    lazy val message: String = err"Expected type: " + expected + err", found: " + found + err"."
  }

  case class ClassDoesntHaveMethod(className: String, methSignature: String, methName: String, alternatives: List[String], override val pos: Positioned)
    extends TypeCheckingError(1, pos) {
    lazy val message = err"Class $className does not contain a method $methSignature.${ suggestion(methName, alternatives) }"
  }

  case class MethodOnWrongType(method: String, tpe: String, override val pos: Positioned)
    extends TypeCheckingError(2, pos) {
    lazy val message = err"Cannot call method $method on type $tpe."
  }

  case class ClassDoesntHaveField(className: String, fieldName: String, alternatives: List[String], override val pos: Positioned)
    extends TypeCheckingError(3, pos) {
    lazy val message = err"Class $className does not contain a field $fieldName.${ suggestion(fieldName, alternatives) }"
  }

  case class FieldOnWrongType(tpe: String, override val pos: Positioned)
    extends TypeCheckingError(4, pos) {
    lazy val message = err"Cannot access field on type $tpe."
  }

  case class NonStaticMethodAsStatic(methodName: String, override val pos: Positioned)
    extends TypeCheckingError(5, pos) {
    lazy val message = err"Trying to call method $methodName statically but the method is not declared as static."
  }

  case class NonStaticMethodFromStatic(methodName: String, override val pos: Positioned)
    extends TypeCheckingError(6, pos) {
    lazy val message = err"Cannot access non-static method $methodName from a static method."
  }

  case class NonStaticFieldAsStatic(fieldName: String, override val pos: Positioned)
    extends TypeCheckingError(7, pos) {
    lazy val message = err"Trying to access field $fieldName statically but the field is not declared as static."
  }

  case class NonStaticFieldFromStatic(fieldName: String, override val pos: Positioned)
    extends TypeCheckingError(8, pos) {
    lazy val message = err"Cannot access non-static field $fieldName from a static method."
  }

  case class InvalidPrivacyAccess(sym: Symbol with Modifiable, clazz: ClassSymbol, callingClass: ClassSymbol, override val pos: Positioned)
    extends TypeCheckingError(9, pos) {
    lazy val message: String = {

      val accessability = sym.accessibility match {
        case Protected() => "protected"
        case Private()   => "private"
        case _           => ???
      }
      val tpe = sym match {
        case f: FieldSymbol  => err"field ${ f.name }"
        case m: MethodSymbol => err"method ${ m.signature }"
        case _               => ???
      }
      val className = clazz.name
      val callingClassName = callingClass.name
      err"Cannot use $accessability " + tpe + err" in class $className from class $callingClassName."
    }

  }

  case class AssignUnit(override val pos: Positioned) extends TypeCheckingError(10, pos) {
    lazy val message: String = err"Cannot assign variable to an expression of type $TUnit."
  }

  // Missing 11, 12

  case class OperatorNotFound(operatorSignature: String, args: List[Type], override val pos: Positioned)
    extends TypeCheckingError(13, pos) {
    lazy val message: String = {
      val classesString = overloadedOperatorClassesString(args)
      classesString + err" define an operator $operatorSignature."
    }

    private def overloadedOperatorClassesString(args: List[Type]) =
      if (args.isEmpty)
        ""
      else if (args.lengthCompare(2) != 0 || args(0) == args(1))
        err"The class ${ args.head } does not"
      else if (!args(0).isInstanceOf[TObject])
        err"The class ${ args(1) } does not"
      else if (!args(1).isInstanceOf[TObject])
        err"The class ${ args(0) } does not"
      else
        err"None of the classes " + args.map(arg => err"$arg").mkString(err" or ")

  }

  case class OperatorWrongReturnType(operatorSignature: String, expected: Type, found: Type, override val pos: Positioned)
    extends TypeCheckingError(14, pos) {
    lazy val message = err"Operator $operatorSignature has wrong return type: expected $expected, found $found."
  }

  case class WrongReturnType(tpe: Type, override val pos: Positioned)
    extends TypeCheckingError(15, pos) {
    lazy val message = err"Expected a return value of type $tpe."
  }

  case class DoesntHaveConstructor(className: String, argTypes: List[Type], override val pos: Positioned)
    extends TypeCheckingError(16, pos) {
    lazy val message = err"Class $className does not contain a constructor $methodSignature."

    private def methodSignature = "new" + argTypes.mkString("(", ", ", ")")
  }

  // Missing 17

  case class NoTypeNoInitializer(variableName: String, override val pos: Positioned)
    extends TypeCheckingError(18, pos) {
    lazy val message = err"Variable $variableName declared with no type or initialization."
  }

  case class ValueMustBeInitialized(variableName: String, override val pos: Positioned)
    extends TypeCheckingError(19, pos) {
    lazy val message = err"Value $variableName is not initialized."

  }

  case class NotOnNonNullable(override val pos: Positioned)
    extends TypeCheckingError(20, pos) {
    lazy val message = err"${ "!" } operator can only be applied to ${ "Bool" } and nullable types."
  }

  case class CantInferTypeRecursiveMethod(override val pos: Positioned)
    extends TypeCheckingError(21, pos) {
    lazy val message = err"Cannot infer type of recursive method."
  }

  case class InvalidIncrementDecrementExpr(override val pos: Positioned)
    extends TypeCheckingError(22, pos) {
    lazy val message: String = err"Invalid increment/decrement expression."
  }

  // Missing 23

  case class InstantiateTrait(treit: String, override val pos: Positioned)
    extends TypeCheckingError(24, pos) {
    lazy val message = err"Cannot instantiate trait $treit."
  }

  case class UnimplementedMethodFromTrait(className: String, unimplementedMethods: List[(MethodSymbol, ClassSymbol)], override val pos: Positioned)
    extends TypeCheckingError(25, pos) {
    lazy val message: String = {
      val methods = formatter.list(unimplementedMethods.map { case (meth, from) =>
        val methSignature = meth.signature
        err"$methSignature from trait $from"
      })
      err"Class $className does not implement the following methods:" + NL + methods
    }
  }

  case class UnimplementedOperatorFromTrait(clazz: String, operator: String, tr: String, override val pos: Positioned)
    extends TypeCheckingError(26, pos) {
    lazy val message = err"Class $clazz does not implement operator $operator from trait $tr."
  }

  case class OverridingMethodDifferentReturnType(meth: MethodSymbol, parentMeth: MethodSymbol)
    extends TypeCheckingError(27, meth) {
    lazy val message: String = {
      val method = meth.signature
      val clazz = meth.classSymbol.name
      val retType = meth.getType
      val parent = parentMeth.classSymbol.name
      val parentType = parentMeth.getType
      err"Overriding method $method in class $clazz has return type $retType while the method in parent $parent has return type $parentType."
    }
  }

  case class CantPrintUnitType(override val pos: Positioned)
    extends TypeCheckingError(28, pos) {
    lazy val message = err"Cannot print an expression of type $TUnit."
  }

  case class NoSuperTypeHasMethod(clazz: String, method: String, override val pos: Positioned)
    extends TypeCheckingError(29, pos) {
    lazy val message = err"No super type of class $clazz implements a method $method."
  }

  case class NoSuperTypeHasField(clazz: String, field: String, override val pos: Positioned)
    extends TypeCheckingError(30, pos) {
    lazy val message = err"No super type of class $clazz has a field $field."
  }

  // Missing 31

  case class ForeachNotIterable(tpe: Type, override val pos: Positioned)
    extends TypeCheckingError(32, pos) {
    lazy val message = err"Type $tpe is not iterable."
  }

  case class AssignNullToNonNullable(tpe: Type, override val pos: Positioned)
    extends TypeCheckingError(33, pos) {
    lazy val message = err"Cannot assign null to non nullable type $tpe."
  }

  case class SafeAccessOnNonNullable(tpe: Type, override val pos: Positioned)
    extends TypeCheckingError(34, pos) {
    lazy val message = err"Cannot use safe access on non nullable type $tpe."
  }

  case class ExtractNullableNonNullable(tpe: Type, override val pos: Positioned)
    extends TypeCheckingError(35, pos) {
    lazy val message = err"Cannot use the nullable extraction operator on non nullable type $tpe."
  }

  case class ElvisOperatorNonNullable(tpe: Type, override val pos: Positioned)
    extends TypeCheckingError(36, pos) {
    lazy val message = err"Cannot use the elvis operator on non-nullable type $tpe."
  }

  case class AssignValueToMethodCall(override val pos: Positioned)
    extends TypeCheckingError(37, pos) {
    lazy val message = err"Cannot assign a value to the result of a method call."
  }

  case class NonNullableEqualsNull(tpe: Type, override val pos: Positioned)
    extends TypeCheckingError(38, pos) {
    lazy val message = err"Cannot check if non nullable type $tpe is null."
  }


  //---------------------------------------------------------------------------------------
  //  Warnings
  //---------------------------------------------------------------------------------------

  case class UnusedPrivateMethod(name: String, override val pos: Positioned)
    extends TypeCheckingWarning(0, pos) {
    lazy val message = err"Private method $name is never used."
  }

  //---------------------------------------------------------------------------------------
  //  Private methods
  //---------------------------------------------------------------------------------------


}
