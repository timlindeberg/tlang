package tlang
package compiler
package analyzer

import tlang.compiler.analyzer.Symbols.{ClassSymbol, _}
import tlang.compiler.ast.Trees._
import tlang.compiler.messages._
import tlang.utils.Positioned

trait NamingErrors extends ErrorHandling {

  def report(error: ErrorMessage): Unit = {
    reporter.report(error)

    error.pos match {
      case id: ClassID    => id.setSymbol(ClassErrorSymbol)
      case id: VariableID => id.setSymbol(VariableErrorSymbol)
      case _              =>
    }
  }

  import errorStringContext._

  private def _isValid(productIterator: Iterator[Any]): Boolean =
    !productIterator.exists {
      case s: Symbolic[_] if s.hasSymbol =>
        val sym = s.getSymbol
        sym == ClassErrorSymbol || sym == VariableErrorSymbol
      case _                             => false
    }

  val ErrorLetters = "N"
  abstract class NameAnalysisError(code: Int, pos: Positioned) extends ErrorMessage(ErrorLetters, code, pos) with Product {
    override def isValid: Boolean = _isValid(productIterator)
  }
  abstract class NameAnalysisWarning(code: Int, pos: Positioned) extends WarningMessage(ErrorLetters, code, pos) with Product {
    override def isValid: Boolean = _isValid(productIterator)
  }

  //---------------------------------------------------------------------------------------
  //  Error messages
  //---------------------------------------------------------------------------------------

  case class InheritanceCycle(set: Set[ClassSymbol], c: ClassSymbol, override val pos: Positioned) extends NameAnalysisError(0, pos) {
    lazy val message = err"A cycle was found in the inheritance graph: " + inheritanceList(set, c)

    private def inheritanceList(set: Set[ClassSymbol], c: ClassSymbol) = {
      val first = if (set.size >= 2)
        set.map(c => err"${ c.name }").mkString(err" <: ")
      else
        err"${ c.name }"
      first + err" <: ${ c.name }"
    }
  }

  case class AbstractOperator(override val pos: Positioned) extends NameAnalysisError(1, pos) {
    lazy val message = err"Operators cannot be abstract."
  }

  // Missing 2

  case class ClassAlreadyDefined(name: String, line: Int, override val pos: Positioned) extends NameAnalysisError(3, pos) {
    lazy val message = err"Class $name is already defined at line $line."
  }

  case class VariableAlreadyDefined(name: String, line: Int, override val pos: Positioned) extends NameAnalysisError(4, pos) {
    lazy val message = err"Variable named $name is already defined at line $line."
  }

  case class FieldDefinedInSuperClass(name: String, override val pos: Positioned) extends NameAnalysisError(5, pos) {
    lazy val message = err"Field $name is already defined in super class."
  }

  case class UnknownType(name: String, alternatives: List[String], override val pos: Positioned) extends NameAnalysisError(6, pos) {
    lazy val message = err"Unknown type: $name.${ suggestion(name, alternatives) }"
  }

  case class MethodAlreadyDefined(methodSignature: String, line: Int, override val pos: Positioned) extends NameAnalysisError(7, pos) {
    lazy val message = err"Method $methodSignature is already defined at line $line."
  }

  // Missing 8

  case class OperatorAlreadyDefined(operator: String, alreadyDefined: Positioned, override val pos: Positioned) extends NameAnalysisError(9, pos) {
    lazy val message = err"Operator $operator is already defined."

    case class OperatorAlreadyDefinedExtraMessage() extends ExtraMessage(alreadyDefined) {
      lazy val message: String = err"Here:"
    }

    override lazy val notes = List(OperatorAlreadyDefinedExtraMessage())
  }

  case class CantResolveSymbol(name: String, alternatives: List[String], override val pos: Positioned) extends NameAnalysisError(10, pos) {
    lazy val message = err"Could not resolve symbol $name.${ suggestion(name, alternatives) }"
  }

  case class AccessNonStaticFromStatic(name: String, override val pos: Positioned) extends NameAnalysisError(11, pos) {
    lazy val message = err"Non-static field $name cannot be accessed from a static function."
  }

  case class ParentNotDeclared(name: String, alternatives: List[String], override val pos: Positioned)
    extends NameAnalysisError(12, pos) {
    lazy val message = err"Could not resolve parent symbol $name.${ suggestion(name, alternatives) }"
  }

  case class ThisInStaticContext(override val pos: Positioned) extends NameAnalysisError(13, pos) {
    lazy val message = err"${ "this" } can not be used in a static context."
  }

  case class OperatorWrongTypes(operatorSignature: String, classSymbol: ClassSymbol, className: String, override val pos: Positioned) extends NameAnalysisError(14, pos) {
    lazy val message = err"Operator $operatorSignature defined in class $classSymbol needs to have $className as an argument."
  }

  case class BreakContinueOutsideLoop(breakOrContinue: String, override val pos: Positioned) extends NameAnalysisError(15, pos) {
    lazy val message = err"Can not use $breakOrContinue statement outside of a loop."
  }

  case class ExtendMultipleClasses(override val pos: Positioned) extends NameAnalysisError(16, pos) {
    lazy val message = err"Can only extend from multiple traits, not classes."
  }

  case class NonFirstArgumentIsClass(override val pos: Positioned) extends NameAnalysisError(17, pos) {
    lazy val message = err"Only the first parent can be a class."
  }

  case class ClassUnimplementedMethod(override val pos: Positioned) extends NameAnalysisError(18, pos) {
    lazy val message = err"Only traits can have unimplemented methods."
  }

  case class AbstractConstructor(override val pos: Positioned) extends NameAnalysisError(19, pos) {
    lazy val message = err"Constructors cannot be abstract."
  }

  case class SuperInStaticContext(override val pos: Positioned) extends NameAnalysisError(20, pos) {
    lazy val message = err"${ "super" } can not be used in a static context."
  }

  case class SuperSpecifierDoesNotExist(inheritedClass: String, clazz: String, override val pos: Positioned) extends NameAnalysisError(21, pos) {
    lazy val message = err"Super refers to class $inheritedClass which $clazz does not inherit from."
  }

  case class NonStaticFinalFieldInTrait(override val pos: Positioned) extends NameAnalysisError(22, pos) {
    lazy val message = err"Fields in traits need to be val static."
  }

  case class UnimplementedMethodNoReturnType(method: String, override val pos: Positioned) extends NameAnalysisError(23, pos) {
    lazy val message = err"Unimplemented method $method needs a return type."
  }

  case class AnnotationNeedsLiteralValue(override val pos: Positioned) extends NameAnalysisError(24, pos) {
    lazy val message = err"Only literal values can be used in an annotation."
  }

  case class StaticInAnnotation(override val pos: Positioned) extends NameAnalysisError(25, pos) {
    lazy val message = err"Cannot use static inside an annotation."
  }

  case class ConstructorInAnnotation(override val pos: Positioned) extends NameAnalysisError(26, pos) {
    lazy val message = err"Cannot declare a constructor inside an annotation."
  }

  case class OperatorInAnnotation(override val pos: Positioned) extends NameAnalysisError(27, pos) {
    lazy val message = err"Cannot declare an operator inside an annotation."
  }

  case class ImplementedMethodInAnnotation(override val pos: Positioned) extends NameAnalysisError(28, pos) {
    lazy val message = err"Methods inside annotations need to be abstract."
  }

  case class InvalidMethodReturnTypeInAnnotation(override val pos: Positioned) extends NameAnalysisError(29, pos) {
    lazy val message: String = {
      val validTypes = Types.AnnotationTypes.map { tpe => err"$tpe" }
      err"Methods inside annotations need to return one of following types:" + NL + formatter.commaOrList(validTypes)
    }
  }

  case class PrivateMethodInAnnotation(override val pos: Positioned) extends NameAnalysisError(30, pos) {
    lazy val message = err"Methods inside annotations need to be public."
  }
  //---------------------------------------------------------------------------------------
  //  Warnings
  //---------------------------------------------------------------------------------------

  case class UnusedVar(variable: VariableSymbol) extends NameAnalysisWarning(0, variable) {
    lazy val message: String = {
      val name = variable.name
      variable match {
        case _: FieldSymbol => err"Private field $name is never used."
        case _              => err"Variable $name is never used."
      }
    }
  }

  // Missing 1
  // Missing 2

  case class UselessStatement(override val pos: Positioned) extends NameAnalysisWarning(3, pos) {
    lazy val message = err"Statement has no effect."
  }

  case class CouldBeVal(value: String, override val pos: Positioned) extends NameAnalysisWarning(4, pos) {
    lazy val message = err"Variable $value could be val."
  }

}
