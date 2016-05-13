package tcompiler.analyzer

import tcompiler.analyzer.Symbols.{ClassSymbol, _}
import tcompiler.ast.Trees.{Break, Tree, _}
import tcompiler.utils.{Errors, Positioned}

/**
  * Created by Tim Lindeberg on 5/13/2016.
  */
trait NameAnalysisErrors extends Errors {

  override val ErrorPrefix = "N"

  def error(errorCode: Int, msg: String, tree: Positioned): Symbol = {
    tree match {
      case id: Identifier      => id.setSymbol(new ErrorSymbol)
      case id: ClassIdentifier => id.setSymbol(new ClassSymbol("ERROR"))
      case _                   =>
    }

    ctx.reporter.error(ErrorPrefix, errorCode, msg, tree)
    new ErrorSymbol()
  }

  //---------------------------------------------------------------------------------------
  //  Error messages
  //---------------------------------------------------------------------------------------

  protected def ErrorInheritanceCycle(set: Set[ClassSymbol], c: ClassSymbol, pos: Positioned) = {
    val list = inheritenceList(set, c)
    error(0, s"A cycle was found in the inheritence graph: $list", pos)
  }

  protected def ErrorOverrideOperator(pos: Positioned) =
    error(1, "Operators cannot be overriden.", pos)

  protected def ErrorClassAlreadyDefined(name: String, line: Int, pos: Positioned) =
    error(3, s"Class '$name' is already defined at line '$line'.", pos)

  protected def ErrorVariableAlreadyDefined(name: String, line: Int, pos: Positioned) =
    error(4, s"Variable '$name' is already defined at line '$line'.", pos)

  protected def ErrorFieldDefinedInSuperClass(name: String, pos: Positioned) =
    error(5, s"Field '$name' is already defined in super class.", pos)

  protected def ErrorUnknownType(name: String, pos: Positioned) =
    error(6, s"Unknown type: '$name'.", pos)

  protected def ErrorMethodAlreadyDefined(methodSignature: String, line: Int, pos: Positioned) =
    error(7, s"Method '$methodSignature' is already defined at line '$line'.", pos)

  protected def ErrorOperatorAlreadyDefined(operator: String, line: Int, pos: Positioned) =
    error(9, s"Operator '$operator' is already defined at line '$line'.", pos)

  protected def ErrorCantResolveSymbol(name: String, pos: Positioned) =
    error(10, s"Could not resolve symbol '$name'.", pos)

  protected def ErrorAccessNonStaticFromStatic(name: String, pos: Positioned) =
    error(11, s"Non-static field '$name' cannot be accessed from a static function.", pos)

  protected def ErrorParentNotDeclared(name: String, pos: Positioned) =
    error(12, s"Parent class '$name' was not declared. ", pos)

  protected def ErrorThisInStaticContext(pos: Positioned) =
    error(13, "'this' can not be used in a static context.", pos)

  protected def ErrorOperatorWrongTypes(operatorType: ExprTree, argTypes: List[String], clazz: String, pos: Positioned) = {
    val op = operatorString(operatorType, argTypes)
    error(14, s"Operator '$op' defined in class '$clazz' needs to have '$clazz' as an argument.", pos)
  }

  protected def ErrorBreakContinueOutsideLoop(stat: Tree, pos: Positioned) = {
    val breakOrContinue = if (stat.isInstanceOf[Break]) "break" else "continue"
    error(15, s"Can not use $breakOrContinue statement outside of a loop.", pos)
  }

  protected def ErrorExtendMultipleClasses(pos: Positioned) =
    error(16, s"Can only extend from multiple traits, not classes.", pos)

  protected def ErrorNonFirstArgumentIsClass(pos: Positioned) =
    error(17, s"Only the first parent can be a class.", pos)

  protected def ErrorClassUnimplementedMethod(pos: Positioned) =
    error(18, s"Only traits can have unimplemented methods.", pos)

  protected def ErrorUnimplementedMethodNoReturnType(method: String, pos: Positioned) =
    error(19, s"Unimplemented method '$method' needs a return type.", pos)

  protected def ErrorAbstractOperator(pos: Positioned) =
    error(19, s"Operators cannot be abstract.", pos)

  protected def ErrorSuperInStaticContext(pos: Positioned) =
    error(20, "'super' can not be used in a static context.", pos)

  protected def ErrorSuperSpecifierDoesNotExist(inheritedClass: String, clazz: String, pos: Positioned) =
    error(21, s"Super refers to class '$inheritedClass' which '$clazz' does not inherit from.", pos)

  protected def ErrorNonStaticFinalFieldInTrait(pos: Positioned) =
    error(22, s"Fields in traits need to be val static.", pos)

  protected def ErrorReassignmentToVal(value: String, pos: Positioned) =
    error(23, s"Cannot reassign value '$value'.", pos)


  //---------------------------------------------------------------------------------------
  //  Warnings
  //---------------------------------------------------------------------------------------

  protected def WarningUnusedVar(v: VariableSymbol) = v.varType match {
    case Field    => WarningUnusedprotectedField(v.name, v)
    case Argument => WarningUnusedArgument(v.name, v)
    case LocalVar => WarningUnusedLocalVar(v.name, v)
  }

  protected def WarningUnusedLocalVar(name: String, pos: Positioned) =
    warning(0, s"Variable '$name' is never used.", pos)

  protected def WarningUnusedArgument(name: String, pos: Positioned) =
    warning(1, s"Argument '$name' is never used.", pos)

  protected def WarningUnusedprotectedField(name: String, pos: Positioned) =
    warning(2, s"protected field '$name' is never used.", pos)

  protected def WarningUselessStatement(pos: Positioned) =
    warning(3, s"Statement has no effect.", pos)

  protected def WarningCouldBeVal(value: String, pos: Positioned) =
    warning(4, s"Variable '$value' could be val.", pos)


  //---------------------------------------------------------------------------------------
  //  Private methods
  //---------------------------------------------------------------------------------------

  private def inheritenceList(set: Set[ClassSymbol], c: ClassSymbol) = {
    val first = if (set.size >= 2)
      set.map(c => s"'${c.name}'").mkString(" <: ")
    else
      c.name
    first + " <: '" + c.name + "'"
  }


}
