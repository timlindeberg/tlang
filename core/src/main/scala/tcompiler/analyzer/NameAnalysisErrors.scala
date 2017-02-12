package tcompiler.analyzer

import tcompiler.analyzer.Symbols.{ClassSymbol, _}
import tcompiler.analyzer.Types.Type
import tcompiler.ast.Trees.{Break, Tree, _}
import tcompiler.error.{ErrorLevel, Errors}
import tcompiler.utils.Positioned

/**
  * Created by Tim Lindeberg on 5/13/2016.
  */
trait NameAnalysisErrors extends Errors {

  override val ErrorLetters = "N"

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
    error(0, err"A cycle was found in the inheritance graph: " + list, pos)
  }

  protected def ErrorAbstractOperator(pos: Positioned): Symbol =
    error(1, err"Operators cannot be abstract.", pos)

  protected def ErrorNullableInOperator(operator: String, pos: Positioned): Symbol =
    error(2, err"Operator $operator cannot have nullable types as arguments or return type.", pos)

  protected def ErrorClassAlreadyDefined(name: String, line: Int, pos: Positioned): Symbol =
    error(3, err"Class $name is already defined at line $line.", pos)

  protected def ErrorVariableAlreadyDefined(name: String, line: Int, pos: Positioned): Symbol =
    error(4, err"Variable named $name is already defined at line $line.", pos)

  protected def ErrorFieldDefinedInSuperClass(name: String, pos: Positioned): Symbol =
    error(5, err"Field $name is already defined in super class.", pos)

  protected def ErrorUnknownType(name: String, alternatives: List[String], pos: Positioned): Symbol =
    error(6, err"Unknown type: $name.${nameSuggestor(name, alternatives)}", pos)

  protected def ErrorMethodAlreadyDefined(methodSignature: String, line: Int, pos: Positioned): Symbol =
    error(7, err"Method $methodSignature is already defined at line $line.", pos)

  // Missing 8

  protected def ErrorOperatorAlreadyDefined(operator: String, line: Int, pos: Positioned): Symbol =
    error(9, err"Operator $operator is already defined at line $line.", pos)

  protected def ErrorCantResolveSymbol(name: String, alternatives: List[String], pos: Positioned): Symbol =
    error(10, err"Could not resolve symbol $name.${nameSuggestor(name, alternatives)}", pos)

  protected def ErrorAccessNonStaticFromStatic(name: String, pos: Positioned): Symbol =
    error(11, err"Non-static field $name cannot be accessed from a static function.", pos)

  protected def ErrorParentNotDeclared(name: String, alternatives: List[String], pos: Positioned): Symbol =
    error(12, err"Could not resolve parent symbol $name.${nameSuggestor(name, alternatives)}", pos)

  protected def ErrorThisInStaticContext(pos: Positioned): Symbol =
    error(13, err"${"this"} can not be used in a static context.", pos)

  protected def ErrorOperatorWrongTypes(operatorType: OperatorTree, argTypes: List[Type], classSymbol: ClassSymbol, className: String, pos: Positioned): Symbol = {
    val op = operatorType.signature(argTypes)
    val classString = classSymbol match {
      case _: ExtensionClassSymbol => err"extension class of $className"
      case _                       => err"class $className"
    }
    error(14, err"Operator $op defined in " + classString + err" needs to have $className as an argument.", pos)
  }

  protected def ErrorBreakContinueOutsideLoop(stat: Tree, pos: Positioned): Symbol = {
    val breakOrContinue = if (stat.isInstanceOf[Break]) "break" else "continue"
    error(15, err"Can not use $breakOrContinue statement outside of a loop.", pos)
  }

  protected def ErrorExtendMultipleClasses(pos: Positioned): Symbol =
    error(16, err"Can only extend from multiple traits, not classes.", pos)

  protected def ErrorNonFirstArgumentIsClass(pos: Positioned): Symbol =
    error(17, err"Only the first parent can be a class.", pos)

  protected def ErrorClassUnimplementedMethod(pos: Positioned): Symbol =
    error(18, err"Only traits can have unimplemented methods.", pos)

  protected def ErrorUnimplementedMethodNoReturnType(method: String, pos: Positioned): Symbol =
    error(19, err"Unimplemented method $method needs a return type.", pos)

  protected def ErrorAbstractConstructor(pos: Positioned): Symbol =
    error(19, err"Constructors cannot be abstract.", pos)

  protected def ErrorSuperInStaticContext(pos: Positioned): Symbol =
    error(20, err"${"super"} can not be used in a static context.", pos)

  protected def ErrorSuperSpecifierDoesNotExist(inheritedClass: String, clazz: String, pos: Positioned): Symbol =
    error(21, err"Super refers to class $inheritedClass which $clazz does not inherit from.", pos)

  protected def ErrorNonStaticFinalFieldInTrait(pos: Positioned): Symbol =
    error(22, err"Fields in traits need to be val static.", pos)

  //---------------------------------------------------------------------------------------
  //  Warnings
  //---------------------------------------------------------------------------------------

  protected def WarningUnusedVar(v: VariableSymbol): Unit = v match {
    case _: FieldSymbol => WarningUnusedPrivateField(v.name, v)
    case _              => WarningUnusedVar(v.name, v)
  }

  protected def WarningUnusedVar(name: String, pos: Positioned): Unit =
    warning(0, err"Variable $name is never used.", pos)

  // Missing 1

  protected def WarningUnusedPrivateField(name: String, pos: Positioned): Unit =
    warning(2, err"Private field $name is never used.", pos)

  protected def WarningUselessStatement(pos: Positioned): Unit =
    warning(3, err"Statement has no effect.", pos)

  protected def WarningCouldBeVal(value: String, pos: Positioned): Unit =
    warning(4, err"Variable $value could be val.", pos)


  //---------------------------------------------------------------------------------------
  //  Private methods
  //---------------------------------------------------------------------------------------

  private def inheritanceList(set: Set[ClassSymbol], c: ClassSymbol) = {
    val first = if (set.size >= 2)
      set.map(c => err"${c.name}").mkString(err" <: ")
    else
      err"${c.name}"
    first + err" <: ${c.name}"
  }

}
