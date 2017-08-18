package tlang.compiler.analyzer

import tlang.compiler.analyzer.Symbols.{ClassSymbol, FieldSymbol, MethodSymbol, OperatorSymbol, Symbol, VariableSymbol}
import tlang.compiler.analyzer.Types.{TError, TObject, TUnit, Type}
import tlang.compiler.ast.Trees.{PostDecrement, PostIncrement, PreDecrement, PreIncrement, _}
import tlang.compiler.error._
import tlang.utils.Positioned

trait TypeCheckingErrors extends ErrorHandling {


  def report(error: CompilerMessage): Type = {
    reporter.report(error)
    TError
  }

  //---------------------------------------------------------------------------------------
  //  Error messages
  //---------------------------------------------------------------------------------------

  import errorStringContext._


  private val ErrorLetters = "T"
  abstract class TypeCheckingError(code: Int, pos: Positioned)
    extends CompilerMessage(MessageType.Error, ErrorLetters, code, pos)
  abstract class TypeCheckingWarning(code: Int, pos: Positioned)
    extends CompilerMessage(MessageType.Warning, ErrorLetters, code, pos)

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
    lazy val message = err"Class $className does not contain a method $methSignature.${ suggestions(methName, alternatives) }"
  }

  case class MethodOnWrongType(method: String, tpe: String, override val pos: Positioned)
    extends TypeCheckingError(2, pos) {
    lazy val message = err"Cannot call method $method on type $tpe."
  }

  case class ClassDoesntHaveField(className: String, fieldName: String, alternatives: List[String], override val pos: Positioned)
    extends TypeCheckingError(3, pos) {
    lazy val message = err"Class $className does not contain a field $fieldName.${ suggestions(fieldName, alternatives) }"
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

      val accessability = sym.accessability match {
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

  // Missing 10, 11, 12

  case class IndexingOperatorNotFound(expr: ArrayOperatorTree, args: List[Type], className: String, override val pos: Positioned)
    extends TypeCheckingError(13, pos) {
    lazy val message: String = {
      val operatorName = expr.operatorString(args, className)
      err"The class $className does not define an operator $operatorName."
    }
  }

  case class OperatorNotFound(op: OperatorTree, args: List[Type], override val pos: Positioned)
    extends TypeCheckingError(13, pos) {
    lazy val message: String = {
      val classesString = overloadedOperatorClassesString(args)
      val operatorName = op.signature(args)
      classesString + err" define an operator $operatorName."
    }

    private def overloadedOperatorClassesString(args: List[Type]) =
      if (args.size != 2 || args(0) == args(1))
        err"The class ${
          args.head
        } does not"
      else if (!args(0).isInstanceOf[TObject])
        err"The class ${
          args(1)
        } does not"
      else if (!args(1).isInstanceOf[TObject])
        err"The class ${
          args(0)
        } does not"
      else
        err"None of the classes " + args.map(arg => err"$arg").mkString(err" or ")

  }

  case class OperatorWrongReturnType(op: OperatorSymbol, expected: Type, found: Type)
    extends TypeCheckingError(14, op) {
    lazy val message = {
      val opSignature = op.signature
      err"Operator $opSignature has wrong return type: expected $expected, found $found."
    }
  }

  case class WrongReturnType(tpe: Type, override val pos: Positioned)
    extends TypeCheckingError(15, pos) {
    lazy val message = err"Expected a return value of type $tpe."
  }

  case class DoesntHaveConstructor(className: String, methodSignature: String, override val pos: Positioned)
    extends TypeCheckingError(16, pos) {
    lazy val message = err"Class $className does not contain a constructor $methodSignature."
  }

  // Missing 17

  case class NoTypeNoInitializer(variable: VariableSymbol)
    extends TypeCheckingError(18, variable) {
    lazy val message = {
      val name = variable.name
      err"Variable $name declared with no type or initialization."
    }
  }

  case class ValueMustBeInitialized(variable: VariableSymbol)
    extends TypeCheckingError(19, variable) {
    lazy val message = {
      val name = variable.name
      err"Value $name is not initialized."
    }
  }

  case class NotOnNonNullable(override val pos: Positioned)
    extends TypeCheckingError(20, pos) {
    lazy val message = err"${ "!" } operator can only be applied to ${ "Bool" } and nullable types."
  }

  case class CantInferTypeRecursiveMethod(override val pos: Positioned)
    extends TypeCheckingError(21, pos) {
    lazy val message = err"Cannot infer type of recursive method."
  }

  case class InvalidIncrementDecrementExpr(expr: ExprTree)
    extends TypeCheckingError(22, expr) {
    lazy val message: String = expr match {
      case _: PreIncrement | _: PostIncrement => err"Invalid increment expression."
      case _: PreDecrement | _: PostDecrement => err"Invalid decrement expression."
    }
  }

  // Missing 23

  case class InstantiateTrait(treit: String, override val pos: Positioned)
    extends TypeCheckingError(24, pos) {
    lazy val message = err"Cannot instantiate trait $treit."
  }

  case class UnimplementedMethodFromTrait(clazz: IDClassDeclTree, unimplementedMethods: List[(MethodSymbol, ClassSymbol)])
    extends TypeCheckingError(25, clazz.id) {
    lazy val message: String = {
      val methods = formatting.makeList(unimplementedMethods.map { case (meth, from) =>
        val methSignature = meth.signature
        err"$methSignature from trait $from"
      })
      val className = clazz.id.name
      err"Class $className does not implement the following methods:" + "\n" + methods
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

  case class ForeachNotIterable(container: ExprTree)
    extends TypeCheckingError(32, container) {
    lazy val message = {
      val tpe = container.getType
      err"Type $tpe is not iterable."
    }
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
