package tcompiler.analyzer

import tcompiler.analyzer.Symbols.{ClassSymbol, _}
import tcompiler.analyzer.Types.Type
import tcompiler.ast.Trees.{Break, Tree, _}
import tcompiler.error.{ErrorLevel, Errors, NameSuggestor}
import tcompiler.utils.Positioned

/**
  * Created by Tim Lindeberg on 5/13/2016.
  */
trait NameAnalysisErrors extends Errors {

  override val ErrorLetters = "N"
  val nameSuggestor = new NameSuggestor

  def error(errorCode: Int, msg: String, pos: Positioned): Symbol = {
    report(errorCode, msg, ErrorLevel.Error, pos)

    pos match {
      case id: ClassID    => id.setSymbol(new ClassSymbol(Errors.ErrorName, false))
      case id: VariableID => id.setSymbol(new VariableSymbol(Errors.ErrorName))
      case _              =>
    }

    ErrorSymbol
  }

  //---------------------------------------------------------------------------------------
  //  Error messages
  //---------------------------------------------------------------------------------------

  protected def ErrorInheritanceCycle(set: Set[ClassSymbol], c: ClassSymbol, pos: Positioned): Symbol = {
    val list = inheritanceList(set, c)
    error(0, s"A cycle was found in the inheritance graph: $list", pos)
  }

  // 1 missing

  protected def ErrorNullableInOperator(pos: Positioned): Symbol =
    error(2, "Operators cannot have nullable types as arguments or return type.", pos)

  protected def ErrorClassAlreadyDefined(name: String, line: Int, pos: Positioned): Symbol =
    error(3, s"Class '$name' is already defined at line '$line'.", pos)

  protected def ErrorVariableAlreadyDefined(name: String, line: Int, pos: Positioned): Symbol =
    error(4, s"Variable named '$name' is already defined at line '$line'.", pos)

  protected def ErrorFieldDefinedInSuperClass(name: String, pos: Positioned): Symbol =
    error(5, s"Field '$name' is already defined in super class.", pos)

  protected def ErrorUnknownType(name: String, alternatives: List[String], pos: Positioned): Symbol =
    error(6, s"Unknown type: '$name'.${nameSuggestor(name, alternatives)}", pos)


  protected def ErrorMethodAlreadyDefined(methodSignature: String, line: Int, pos: Positioned): Symbol =
    error(7, s"Method '$methodSignature' is already defined at line '$line'.", pos)

  // Missing 8

  protected def ErrorOperatorAlreadyDefined(operator: String, line: Int, pos: Positioned): Symbol =
    error(9, s"Operator '$operator' is already defined at line '$line'.", pos)

  protected def ErrorCantResolveSymbol(name: String, alternatives: List[String], pos: Positioned): Symbol =
    error(10, s"Could not resolve symbol '$name'.${nameSuggestor(name, alternatives)}", pos)

  protected def ErrorAccessNonStaticFromStatic(name: String, pos: Positioned): Symbol =
    error(11, s"Non-static field '$name' cannot be accessed from a static function.", pos)

  protected def ErrorParentNotDeclared(name: String, alternatives: List[String], pos: Positioned): Symbol =
    error(12, s"Could not resolve parent symbol '$name'.${nameSuggestor(name, alternatives)}", pos)

  protected def ErrorThisInStaticContext(pos: Positioned): Symbol =
    error(13, "'this' can not be used in a static context.", pos)

  protected def ErrorOperatorWrongTypes(operatorType: OperatorTree, argTypes: List[Type], classSymbol: ClassSymbol, className: String, pos: Positioned): Symbol = {
    val op = operatorType.signature(argTypes)
    val classString = classSymbol match {
      case _: ExtensionClassSymbol => s"extension class of '$className'"
      case _                       => s"class '$className'"
    }
    error(14, s"Operator '$op' defined in $classString needs to have '$className' as an argument.", pos)
  }

  protected def ErrorBreakContinueOutsideLoop(stat: Tree, pos: Positioned): Symbol = {
    val breakOrContinue = if (stat.isInstanceOf[Break]) "break" else "continue"
    error(15, s"Can not use '$breakOrContinue' statement outside of a loop.", pos)
  }

  protected def ErrorExtendMultipleClasses(pos: Positioned): Symbol =
    error(16, s"Can only extend from multiple traits, not classes.", pos)

  protected def ErrorNonFirstArgumentIsClass(pos: Positioned): Symbol =
    error(17, s"Only the first parent can be a class.", pos)

  protected def ErrorClassUnimplementedMethod(pos: Positioned): Symbol =
    error(18, s"Only traits can have unimplemented methods.", pos)

  protected def ErrorUnimplementedMethodNoReturnType(method: String, pos: Positioned): Symbol =
    error(19, s"Unimplemented method '$method' needs a return type.", pos)

  protected def ErrorAbstractOperator(pos: Positioned): Symbol =
    error(19, s"Operators cannot be abstract.", pos)

  protected def ErrorSuperInStaticContext(pos: Positioned): Symbol =
    error(20, "'super' can not be used in a static context.", pos)

  protected def ErrorSuperSpecifierDoesNotExist(inheritedClass: String, clazz: String, pos: Positioned): Symbol =
    error(21, s"Super refers to class '$inheritedClass' which '$clazz' does not inherit from.", pos)

  protected def ErrorNonStaticFinalFieldInTrait(pos: Positioned): Symbol =
    error(22, s"Fields in traits need to be val static.", pos)

  //---------------------------------------------------------------------------------------
  //  Warnings
  //---------------------------------------------------------------------------------------

  protected def WarningUnusedVar(v: VariableSymbol): Unit = v match {
    case _: FieldSymbol => WarningUnusedPrivateField(v.name, v)
    case _              => WarningUnusedVar(v.name, v)
  }

  protected def WarningUnusedVar(name: String, pos: Positioned): Unit =
    warning(0, s"Variable '$name' is never used.", pos)

  // Missing 1

  protected def WarningUnusedPrivateField(name: String, pos: Positioned): Unit =
    warning(2, s"Private field '$name' is never used.", pos)

  protected def WarningUselessStatement(pos: Positioned): Unit =
    warning(3, s"Statement has no effect.", pos)

  protected def WarningCouldBeVal(value: String, pos: Positioned): Unit =
    warning(4, s"Variable '$value' could be val.", pos)


  //---------------------------------------------------------------------------------------
  //  Private methods
  //---------------------------------------------------------------------------------------

  private def inheritanceList(set: Set[ClassSymbol], c: ClassSymbol) = {
    val first = if (set.size >= 2)
      set.map(c => s"'${c.name}'").mkString(" <: ")
    else
      c.name
    s"$first <: '${c.name}'"
  }

}
