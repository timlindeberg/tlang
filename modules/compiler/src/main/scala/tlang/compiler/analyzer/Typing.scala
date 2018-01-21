package tlang.compiler
package analyzer

import tlang.compiler.analyzer.Naming.createErrorStringContext
import tlang.compiler.analyzer.Symbols._
import tlang.compiler.analyzer.Types._
import tlang.compiler.ast.Trees._
import tlang.compiler.imports.Imports
import tlang.compiler.messages.Reporter
import tlang.compiler.utils.DebugOutputFormatter
import tlang.formatting.{ErrorStringContext, Formatting}
import tlang.utils.Extensions._
import tlang.utils.{Logging, Positioned}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object Typing extends CompilerPhase[CompilationUnit, CompilationUnit] with Logging {

  val hasBeenTypechecked: mutable.Set[MethodSymbol]  = mutable.Set()
  var methodUsage       : Map[MethodSymbol, Boolean] = Map()

  val emptyClassSym = new ClassSymbol("")
  val emptyMethSym  = new MethodSymbol("", emptyClassSym, None, Set())

  def run(ctx: Context)(cus: List[CompilationUnit]): List[CompilationUnit] = {
    ctx.executor.foreach(cus) { typecheckFields(ctx, _) }
    ctx.executor.foreach(cus) { typecheckMethods(ctx, _) }
    ctx.executor.foreach(cus) { verify(ctx, _) }

    cus
  }

  override def description(formatting: Formatting): String =
    "Performs type checking and attaches types to trees."

  override def printDebugOutput(output: List[CompilationUnit], debugOutputFormatter: DebugOutputFormatter): Unit =
    debugOutputFormatter.printASTs(phaseName, output)

  private def typecheckFields(ctx: Context, cu: CompilationUnit): Unit = {
    info"Typechecking fields of ${ cu.sourceName }"
    cu.classes.foreach { classDecl =>
      val typeChecker = TypeChecker(ctx, cu, new MethodSymbol("", classDecl.getSymbol, None, Set()))
      classDecl.fields.foreach(typeChecker.tcStat(_))
    }
  }


  private def typecheckMethods(ctx: Context, cu: CompilationUnit): Unit = {
    info"Typechecking methods of ${ cu.sourceName }"
    cu.classes.flatMap(_.methods).foreach { method =>
      val methodSymbol = method.getSymbol
      if (!methodUsage.contains(methodSymbol))
        methodUsage += methodSymbol -> !method.accessability.isInstanceOf[Private]
      TypeChecker(ctx, cu, methodSymbol).tcMethod()
    }
  }

  private def verify(ctx: Context, cu: CompilationUnit): Unit = {
    info"Verifying types of ${ cu.sourceName }"
    val typeChecker = TypeChecker(ctx, cu, emptyMethSym)
    typeChecker.checkMethodUsage()
    typeChecker.checkCorrectOverrideReturnTypes(cu)
    typeChecker.checkTraitsAreImplemented(cu)
  }

}


object TypeChecker {
  def apply(ctx: Context, cu: CompilationUnit, methodSymbol: MethodSymbol): TypeChecker = TypeChecker(
    ctx.reporter,
    createErrorStringContext(ctx, cu),
    cu.imports,
    methodSymbol,
    List()
  )
}
case class TypeChecker(
  override val reporter: Reporter,
  override val errorStringContext: ErrorStringContext,
  imports: Imports,
  currentMethodSymbol: MethodSymbol,
  methodStack: List[MethodSymbol] = List()) extends TypingErrors {

  override def replaceNames(str: String): String = imports.replaceNames(str)

  import Typing._

  val returnStatements: ArrayBuffer[(Return, Type)] = ArrayBuffer()

  def tcMethod(): Unit = {
    if (Typing.hasBeenTypechecked(currentMethodSymbol))
      return

    debug"Typechecking method $currentMethodSymbol"

    if (currentMethodSymbol.getType == TUntyped && methodStack.contains(currentMethodSymbol)) {
      report(CantInferTypeRecursiveMethod(currentMethodSymbol))
      return
    }

    currentMethodSymbol.stat.ifDefined(tcStat)
    hasBeenTypechecked += currentMethodSymbol

    val methType = currentMethodSymbol.getType
    if (methType != TUntyped) {
      returnStatements.map(_._1).foreach(_.setType(methType))
      checkOperatorType(methType)
      return
    }

    if (returnStatements.isEmpty) {
      currentMethodSymbol.setType(TUnit)
      return
    }

    val returnTypes = returnStatements.map(_._2)
    val inferredType = getReturnType(returnTypes)
    returnStatements.map(_._1) foreach { _.setType(inferredType) }

    checkOperatorType(inferredType)
    currentMethodSymbol.setType(inferredType)
  }

  def checkOperatorType(tpe: Type): Unit = currentMethodSymbol match {
    case op: OperatorSymbol =>
      // Special rules for some operators
      val correctOperatorType = op.operatorType match {
        case Assign(ArrayRead(_, _), _) => TUnit
        case Hash(_)                    => Int
        case ComparisonOperatorTree(_, _) |
             EqualsOperatorTree(_, _)   => Bool
        case _                          => return
      }
      if (tpe != correctOperatorType)
        report(OperatorWrongReturnType(op.signature, correctOperatorType, tpe, op))
    case _                  =>
  }

  def tcStat(statement: StatTree): Unit = statement match {
    case Block(stats)                      =>
      stats.foreach(tcStat)
    case VarDecl(id, tpe, init, _)         =>
      val varSym = id.getSymbol
      if (varSym.isFinal && init.isEmpty)
        report(ValueMustBeInitialized(varSym.name, varSym))

      (tpe, init) match {
        case (Some(tpe), Some(expr)) => tcExpr(expr, tpe.getType)
        case (None, Some(expr))      => id.setType(tcExpr(expr))
        case (Some(_), None)         => // Abstract
        case (None, None)            => report(NoTypeNoInitializer(varSym.name, varSym))
      }
      init.ifDefined(expr =>
        if (expr.getType == TUnit)
          report(AssignUnit(expr))
      )
    case If(condition, thn, els)           =>
      tcExpr(condition, Bool)
      tcStat(thn)
      els ifDefined tcStat
    case While(condition, stat)            =>
      tcExpr(condition, Bool)
      tcStat(stat)
    case For(init, condition, post, stat)  =>
      init foreach tcStat
      tcExpr(condition, Bool)
      post foreach tcStat
      tcStat(stat)
    case Foreach(varDecl, container, stat) =>
      val containerType = tcExpr(container)
      val expectedVarType = containerType match {
        case TArray(arrTpe)       =>
          arrTpe
        case TObject(classSymbol) =>
          getIteratorType(classSymbol) getOrElse report(ForeachNotIterable(container.getType, container))
        case _                    => report(ForeachNotIterable(container.getType, container))
      }
      varDecl.id.getType match {
        case TUntyped                      => varDecl.id.setType(expectedVarType)
        case tpe if tpe != expectedVarType => report(WrongType(expectedVarType, tpe, varDecl.id))
        case _                             =>
      }
      tcStat(stat)
    case PrintStatTree(expr)               =>
      tcExpr(expr)
      if (expr.getType == TUnit)
        report(CantPrintUnitType(expr))
    case Error(expr)                       =>
      tcExpr(expr, String)
    case ret@Return(Some(expr))            =>
      val retType = currentMethodSymbol.getType match {
        case TUntyped => tcExpr(expr)
        case retType  => tcExpr(expr, retType)
      }
      returnStatements += ((ret, retType))
    case ret@Return(None)                  =>
      if (currentMethodSymbol.getType != TUntyped && currentMethodSymbol.getType != TUnit)
        report(WrongReturnType(currentMethodSymbol.getType, ret))
      returnStatements += ((ret, TUnit))
    case _: Break | _: Continue            =>
    case expr: ExprTree                    =>
      tcExpr(expr)
  }

  def tcExpr(expr: ExprTree, expected: Type*): Type = tcExpr(expr, expected.toList)

  def tcExpr(expression: ExprTree, expected: Traversable[Type]): Type = {
    val foundType = expression match {
      case lit: Literal[_]                            => lit.getType
      case id: VariableID                             =>
        id.getSymbol.ifInstanceOf[FieldSymbol] { fieldSymbol =>
          checkPrivacy(fieldSymbol, fieldSymbol.classSymbol, id)
        }
        id.getType
      case id: ClassID                                => id.getType
      case th: This                                   => th.getSymbol.getType
      case su: Super                                  => su.getSymbol.getType
      case acc: Access                                => tcAccess(acc)
      case assign: Assign                             =>
        tcAssignment(assign)
      case newDecl: New                               => tcNewExpr(newDecl)
      case NewArray(tpe, sizes)                       =>
        sizes.foreach(tcExpr(_, Int))
        tpe.getType
      case ArrayLit(expressions)                      =>
        val tpes = expressions.map(tcExpr(_))
        val inferredType = getReturnType(tpes)

        // An empty array literal should have Object type
        val tpe = if (inferredType == TUnit) Object else inferredType
        TArray(tpe)
      case And(lhs, rhs)                              =>
        tcExpr(lhs, Bool)
        tcExpr(rhs, Bool)
        Bool
      case Or(lhs, rhs)                               =>
        tcExpr(lhs, Bool)
        tcExpr(rhs, Bool)
        Bool
      case notOp@Not(expr)                            =>
        val tpe = tcExpr(expr)
        if (!tpe.isNullable && tpe != Bool)
          report(NotOnNonNullable(notOp))

        Bool
      case eqOp@EqualsOperatorTree(lhs, rhs)          =>
        val lhsTpe = tcExpr(lhs)
        val rhsTpe = tcExpr(rhs)

        if (lhsTpe == TNull || rhsTpe == TNull) {
          if (lhsTpe == TNull && !rhsTpe.isNullable || rhsTpe == TNull && !lhsTpe.isNullable)
            report(NonNullableEqualsNull(rhs.getType, eqOp))
          Bool
        } else if (lhsTpe.isInstanceOf[TArray] && rhsTpe.isInstanceOf[TArray]) {
          Bool
        } else {
          tcBinaryOperator(eqOp, lhsTpe, rhsTpe)
        }
      case binOp@BinaryOperatorTree(lhs, rhs)         =>
        tcBinaryOperator(binOp, tcExpr(lhs), tcExpr(rhs))
      case incOp@IncrementDecrementTree(obj)          =>
        if (!obj.isInstanceOf[Assignable])
          report(InvalidIncrementDecrementExpr(incOp))

        tcUnaryOperator(incOp, tcExpr(obj))
      case ExtractNullable(expr)                      =>
        val exprTpe = tcExpr(expr)
        if (!exprTpe.isNullable)
          report(ExtractNullableNonNullable(exprTpe, expr))
        exprTpe.getNonNullable
      case unaryOp@UnaryOperatorTree(expr)            =>
        tcUnaryOperator(unaryOp, tcExpr(expr))
      case Is(expr, _)                                =>
        tcExpr(expr)
        Bool
      case As(expr, tpe)                              =>
        tcExpr(expr, Object)
        tpe.getType
      case arrRead@ArrayRead(arr, index)              =>
        val arrayType = tcExpr(arr)
        arrayType match {
          case _: TObject     =>
            val indexType = tcExpr(index)
            tcArrayOperator(arrayType, arrRead, List(indexType), arrayType, expression)
          case TArray(arrTpe) =>
            tcExpr(index, Int)
            arrTpe
          case TError         => TError
          case _              => ???
        }
      case arrSlice@ArraySlice(obj, start, end, step) =>
        val argTypes = List(start, end, step).map {
          case Some(expr) => tcExpr(expr, Int)
          case None       => TNull
        }
        tcExpr(obj) match {
          case objectType: TObject => tcArrayOperator(objectType, arrSlice, argTypes, objectType, expression)
          case arrayType: TArray   => arrayType
          case TError              => TError
          case _                   => ???
        }
      case Ternary(condition, thn, els)               =>
        tcExpr(condition, Bool)
        val thnType = tcExpr(thn)
        val elsType = tcExpr(els)
        getReturnType(List(thnType, elsType))
      case Elvis(nullableValue, ifNull)               =>
        val nullableTpe = tcExpr(nullableValue)
        if (!nullableTpe.isNullable)
          report(ElvisOperatorNonNullable(nullableTpe, nullableValue))
        tcExpr(ifNull, nullableTpe)
        nullableTpe.getNonNullable
    }

    def correctType(expectedTpe: Type): Boolean = {
      (expectedTpe == Bool && foundType.isNullable) ||
        foundType.isSubTypeOf(expectedTpe) ||
        expectedTpe.isImplicitlyConvertibleFrom(foundType)
    }

    val res = if (expected.nonEmpty && !expected.exists(correctType))
      report(WrongType(expected, foundType, expression))
    else
      foundType

    debug"Setting type of $expression at (${ expression.line }:${ expression.col }) to $res"
    expression.setType(res)
    res
  }

  def tcNewExpr(newExpr: New): Type = {
    val tpe = newExpr.tpe
    val exprs = newExpr.args
    val argTypes = exprs.map(tcExpr(_))
    tpe.getType match {
      case TObject(classSymbol) =>
        if (classSymbol.isAbstract) {
          report(InstantiateTrait(classSymbol.name, newExpr))
        } else {
          classSymbol.lookupMethod("new", argTypes, imports) match {
            case Some(constructorSymbol) =>
              checkPrivacy(constructorSymbol, classSymbol, newExpr)
              newExpr.setSymbol(constructorSymbol)
            case None if exprs.nonEmpty  =>
              val methodSignature = "new" + exprs.map(_.getType).mkString("(", ", ", ")")
              report(DoesntHaveConstructor(tpe.name, methodSignature, newExpr))
            case _                       =>
              val defaultConstructor = new MethodSymbol("new", classSymbol, None, Set(Public()))
              newExpr.setSymbol(defaultConstructor)
          }
        }
      case _                    => ???
    }
    tpe.getType
  }

  def tcAccess(acc: Access): Type = {
    val app = acc.application

    // If it's a method access we type check it now
    val argTypes = app match {
      case MethodCall(_, args) => Some(args.map(tcExpr(_)))
      case _                   => None
    }

    def methSignature = {
      val methodCall = app.asInstanceOf[MethodCall]
      methodCall.meth.name + argTypes.get.mkString("(", ", ", ")")
    }

    val objType = tcAccessObject(acc, argTypes, methSignature)

    // Most of the duplication here is to give a different error message for
    // fields and methods
    val tpe = app match {
      case MethodCall(meth, args)        =>
        val tpe = objType match {
          case TObject(classSymbol) =>
            classSymbol.lookupMethod(meth.name, argTypes.get, imports) map { methSymbol =>
              checkPrivacy(methSymbol, classSymbol, app)
              checkStaticMethodConstraints(acc, classSymbol, methSymbol, app)
              Typing.methodUsage += methSymbol -> true
              inferTypeOfMethod(methSymbol)
              meth.setSymbol(methSymbol)
              meth.getType
            } getOrElse {
              val alternatives = classSymbol.methods.filter(_.argTypes == argTypes.get).map(_.name)
              report(ClassDoesntHaveMethod(classSymbol.name, methSignature, meth.name, alternatives, app))
            }
          case _: TArray            =>
            if (args.nonEmpty || meth.name != "Size")
              report(MethodOnWrongType(methSignature, objType.toString, app))
            meth.setSymbol(new MethodSymbol("Size", new ClassSymbol("Array"), None, Set()).setType(Int))
            Int
          case _                    => TError
        }
        app.setType(tpe)
        tpe
      case fieldId@VariableID(fieldName) =>
        objType match {
          case TObject(classSymbol) =>
            classSymbol.lookupField(fieldName) map { fieldSymbol =>
              checkPrivacy(fieldSymbol, classSymbol, app)
              checkStaticFieldConstraints(acc, classSymbol, fieldSymbol, app)
              fieldId.setSymbol(fieldSymbol)
              fieldId.getType
            } getOrElse {
              val alternatives = classSymbol.fields.keys.toList
              report(ClassDoesntHaveField(classSymbol.name, fieldName, alternatives, app))
            }
          case _: TArray            =>
            // There are no fields on arrays
            report(ClassDoesntHaveField(objType.toString, fieldName, Nil, app))
          case _                    =>
            TError
        }
    }


    acc match {
      case _: SafeAccess   =>
        if (!objType.isNullable)
          report(SafeAccessOnNonNullable(objType, acc))
        tpe.getNullable
      case _: NormalAccess => tpe
    }
  }


  def getReturnType(tpes: Traversable[Type]): Type = {
    var uniqueTpes = tpes.toSet
    if (uniqueTpes.isEmpty)
      return TUnit

    if (uniqueTpes.size == 1)
      return uniqueTpes.head

    val containsNull = uniqueTpes.contains(TNull)
    uniqueTpes = uniqueTpes.filter(_ notIn List(TNull, TUnit))
    val tpe =
      if (uniqueTpes.size == 1) {
        uniqueTpes.head
      } else if (uniqueTpes.exists(_ in Primitives)) {
        // More than one type and at least one is a primitive
        Object
      } else {
        val s = uniqueTpes.head.getSuperTypes
        val commonTypes = uniqueTpes.drop(1).foldLeft(s) { (common, tpe) => common.intersect(tpe.getSuperTypes) }
        commonTypes.head
      }

    if (containsNull)
      tpe.getNullable
    else
      tpe
  }

  private def tcAccessObject(acc: Access, argTypes: Option[List[Type]], methSignature: => String): Type = {
    val obj = acc.obj
    val app = acc.application

    // Set type of super call based on operation used
    // if it needs to be looked up dynamically
    acc.obj match {
      case sup@Super(None) if sup.getSymbol.parents.nonEmpty =>
        // supers symbol is set to this in name analyzer so we can look up the
        // desired method or field
        val thisSymbol = sup.getSymbol
        val classSymbol = app match {
          case MethodCall(meth, _)   =>
            thisSymbol.lookupParentMethod(meth.name, argTypes.get, imports) map {
              _.classSymbol
            } getOrElse {
              return report(NoSuperTypeHasMethod(thisSymbol.name, methSignature, app))
            }
          case Identifier(fieldName) =>
            thisSymbol.lookupParentField(fieldName).map {
              _.classSymbol
            } getOrElse {
              return report(NoSuperTypeHasField(thisSymbol.name, fieldName, app))
            }
          case _                     => ???
        }
        sup.setSymbol(classSymbol)
        sup.setType(classSymbol.getType)
        classSymbol.getType
      case _                                                 => tcExpr(obj)
    }
  }

  def tcAssignment(assignment: Assign): Type = {
    val to = assignment.to
    val expr = assignment.from

    val tpe = to match {
      case _: VariableID          =>
        val toTpe = tcExpr(to)

        tcExpr(expr, toTpe)
        toTpe
      case Access(_, application) =>
        application match {
          case _: MethodCall => report(AssignValueToMethodCall(assignment))
          case _: VariableID =>
            val toTpe = tcExpr(to)
            tcExpr(expr, toTpe)
            toTpe
          case _             => ???
        }
      case ArrayRead(arr, index)  =>
        val arrTpe = tcExpr(arr)
        to.setType(arr)
        arrTpe match {
          case _: TObject       =>
            val indexType = tcExpr(index)
            val exprType = tcExpr(expr)

            val argList = List(indexType, exprType)
            tcArrayOperator(arrTpe, assignment, argList, arrTpe, assignment)
            exprType
          case TArray(arrayTpe) =>
            tcExpr(index, Int)
            tcExpr(expr, arrayTpe)
            arrTpe.asInstanceOf[TArray].tpe
          case _                => ???
        }
    }
    if (expr.getType == TUnit)
      report(AssignUnit(expr))
    tpe
  }

  def tcBinaryOperator(operator: OperatorTree, arg1: Type, arg2: Type): Type = {
    val argList = List(arg1, arg2)
    typeCheckOperator(arg1, operator, argList)
      .orElse(typeCheckOperator(arg2, operator, argList))
      .getOrElse(report(OperatorNotFound(operator.signature(argList), argList, operator)))
  }

  def tcUnaryOperator(expr: OperatorTree, arg: Type): Type = {
    val argList = List(arg)
    typeCheckOperator(arg, expr, argList)
      .getOrElse(report(OperatorNotFound(expr.signature(argList), argList, expr)))
  }

  def tcArrayOperator(classTpe: Type, opType: ArrayOperatorTree, argList: List[Type], arrTpe: Type, pos: Positioned): Type = {
    typeCheckOperator(classTpe, opType, argList)
      .getOrElse(report(OperatorNotFound(opType.operatorString(argList, arrTpe.toString), List(arrTpe), pos)))
  }

  def checkMethodUsage(): Unit = {
    // Check method usage
    // TODO: Refactoring of typechecker global variables etc.
    methodUsage
      .filter { case (_, used) => !used }
      .foreach { case (method, _) => report(UnusedPrivateMethod(method.signature, method)) }
    methodUsage = Map[MethodSymbol, Boolean]()
  }

  def checkCorrectOverrideReturnTypes(cu: CompilationUnit): Unit =
    cu.classes.foreach { clazz =>
      clazz.methods.foreach { meth =>
        val classSymbol = clazz.getSymbol
        val methSymbol = meth.getSymbol
        val methType = meth.getSymbol.getType
        classSymbol.lookupParentMethod(methSymbol.name, methSymbol.argTypes, imports)
          .filter { parentMeth => parentMeth.getType != methType && !methType.isSubTypeOf(parentMeth.getType) }
          .foreach { parentMeth => report(OverridingMethodDifferentReturnType(methSymbol, parentMeth)) }
      }
    }


  def checkTraitsAreImplemented(cu: CompilationUnit): Unit =
    cu.classes.filterInstance[ClassDecl].foreach { classDecl =>
      classDecl.traits.foreach(t => checkTraitIsImplemented(classDecl, t.getSymbol))
    }

  private def typeCheckOperator(classType: Type, operator: OperatorTree, args: List[Type]): Option[Type] =
    operator.lookupOperator(classType, args, imports).map { operatorSymbol =>
      val classSymbol = classType.asInstanceOf[TObject].classSymbol
      checkPrivacy(operatorSymbol, classSymbol, operator)
      inferTypeOfMethod(operatorSymbol)
      operator.setType(operatorSymbol.getType)
      operatorSymbol.getType
    }

  private def checkTraitIsImplemented(classDecl: IDClassDeclTree, implementedTrait: ClassSymbol) = {
    val classSymbol = classDecl.getSymbol
    val unimplementedMethods = implementedTrait.abstractMethods()
      .filter { case (method, _) => !classSymbol.implementsMethod(method) }

    if (unimplementedMethods.nonEmpty)
      report(UnimplementedMethodFromTrait(classDecl.id.name, unimplementedMethods, classDecl))
  }


  /**
    * This is hardcoded and does not depend on the trait Iterable.
    * This allows for classes which do not implement the Iterable trait
    * but which does provide an Iterator method which returns an Iterator
    * with the methods HasNext and Next of correct types to still be
    * applicable for ForEach loops.
    *
    * This is useful since it allows for iterable behaviour to be added
    * through extension methods.
    */
  private def getIteratorType(classSymbol: ClassSymbol): Option[Type] =
    classSymbol.lookupMethod("Iterator", Nil, imports).flatMap { methSymbol =>
      inferTypeOfMethod(methSymbol) match {
        case TObject(methodClassSymbol) =>
          methodClassSymbol.lookupMethod("HasNext", Nil, imports)
            .filter { inferTypeOfMethod(_) == Bool }
            .flatMap { _ =>
              methodClassSymbol.lookupMethod("Next", Nil, imports).map(inferTypeOfMethod)
            }
        case _                          => None
      }
    }

  private def inferTypeOfMethod(methodSymbol: MethodSymbol): Type = {
    if (methodSymbol.getType == TUntyped)
      TypeChecker(reporter, errorStringContext, imports, methodSymbol, currentMethodSymbol :: methodStack).tcMethod()
    methodSymbol.getType
  }

  private def checkStaticMethodConstraints(acc: Access, classSymbol: ClassSymbol, methodSymbol: MethodSymbol, pos: Positioned) = {
    if (!methodSymbol.isStatic && acc.isStatic)
      report(NonStaticMethodAsStatic(methodSymbol.name, pos))

    if (acc.obj.isInstanceOf[This] && currentMethodSymbol.isStatic && !methodSymbol.isStatic)
      report(NonStaticMethodFromStatic(methodSymbol.name, pos))
  }

  private def checkStaticFieldConstraints(acc: Access, classSymbol: ClassSymbol, varSymbol: FieldSymbol, pos: Positioned) = {
    if (!varSymbol.isStatic && acc.isStatic)
      report(NonStaticFieldAsStatic(varSymbol.name, pos))

    if (acc.obj.isInstanceOf[This] && currentMethodSymbol.isStatic && !varSymbol.isStatic)
      report(NonStaticFieldFromStatic(varSymbol.name, pos))
  }

  private def checkPrivacy(sym: Symbol with Modifiable, classSymbol: ClassSymbol, pos: Positioned) = {
    if (!isValidAccess(classSymbol, sym.accessability))
      report(InvalidPrivacyAccess(sym, classSymbol, currentMethodSymbol.classSymbol, pos))
  }

  private def isValidAccess(classSymbol: ClassSymbol, access: Accessability) = access match {
    case Public()                                                                                => true
    case Private() if classSymbol == currentMethodSymbol.classSymbol                             => true
    case Protected() if currentMethodSymbol.classSymbol.getType.isSubTypeOf(classSymbol.getType) => true
    case _                                                                                       => false
  }
}
