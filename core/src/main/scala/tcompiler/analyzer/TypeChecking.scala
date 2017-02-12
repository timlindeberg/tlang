package tcompiler
package analyzer

import tcompiler.analyzer.Symbols._
import tcompiler.analyzer.Types._
import tcompiler.ast.Trees._
import tcompiler.imports.ImportMap
import tcompiler.utils.Extensions._
import tcompiler.utils._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object TypeChecking extends Pipeline[List[CompilationUnit], List[CompilationUnit]] {

  val hasBeenTypechecked: mutable.Set[MethodSymbol]  = mutable.Set()
  var methodUsage       : Map[MethodSymbol, Boolean] = Map()

  val emptyClassSym = new ClassSymbol("", false)
  val emptyMethSym  = new MethodSymbol("", emptyClassSym, None, Set())

  /**
    * Typechecking does not produce a value, but has the side effect of
    * attaching types to trees and potentially outputting error messages.
    */
  def run(ctx: Context)(cus: List[CompilationUnit]): List[CompilationUnit] = {
    cus foreach {typecheckFields(ctx, _)}
    cus foreach {typecheckMethods(ctx, _)}
    cus foreach {verify(ctx, _)}

    cus
  }

  private def typecheckFields(ctx: Context, cu: CompilationUnit): Unit =
    cu.classes.foreach { classDecl =>
      val typeChecker = new TypeChecker(ctx, cu.importMap, new MethodSymbol("", classDecl.getSymbol, None, Set()))
      classDecl.fields.foreach(typeChecker.tcStat(_))
    }

  private def typecheckMethods(ctx: Context, cu: CompilationUnit): Unit = {
    cu.classes.foreach { classDecl =>
      classDecl.methods.foreach { method =>
        val methodSymbol = method.getSymbol
        if (!methodUsage.contains(methodSymbol))
          methodUsage += methodSymbol -> !method.accessability.isInstanceOf[Private]
        new TypeChecker(ctx, cu.importMap, methodSymbol).tcMethod()
      }
    }
  }

  private def verify(ctx: Context, cu: CompilationUnit): Unit = {
    val typeChecker = new TypeChecker(ctx, cu.importMap, emptyMethSym)
    typeChecker.checkMethodUsage()
    typeChecker.checkCorrectOverrideReturnTypes(cu)
    typeChecker.checkTraitsAreImplemented(cu)
  }

}

class TypeChecker(override var ctx: Context,
  override var importMap: ImportMap,
  currentMethodSymbol: MethodSymbol,
  methodStack: List[MethodSymbol] = List()) extends TypeCheckingErrors {

  import TypeChecking._

  val returnStatements: ArrayBuffer[(Return, Type)] = ArrayBuffer()

  def tcMethod(): Unit = {
    if (TypeChecking.hasBeenTypechecked(currentMethodSymbol))
      return

    if (currentMethodSymbol.getType == TUntyped && methodStack.contains(currentMethodSymbol)) {
      ErrorCantInferTypeRecursiveMethod(currentMethodSymbol)
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
    returnStatements.map(_._1) foreach {_.setType(inferredType)}

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
        ErrorOperatorWrongReturnType(op.signature, correctOperatorType.toString, tpe.toString, op)
    case _                  =>
  }

  def tcStat(statement: StatTree): Unit = statement match {
    case Block(stats)                      =>
      stats.foreach(tcStat)
    case varDecl@VarDecl(tpe, id, init, _) =>
      val varSym = id.getSymbol
      if (varSym.isFinal && init.isEmpty)
        ErrorValueMustBeInitialized(varSym.name, varDecl)

      (tpe, init) match {
        case (Some(tpe), Some(expr)) => tcExpr(expr, tpe.getType)
        case (None, Some(expr))      => id.setType(tcExpr(expr))
        case (Some(tpe), None)       => // Abstract
        case (None, None)            => ErrorNoTypeNoInitalizer(varSym.name, varDecl)
      }
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
          getIteratorType(classSymbol) getOrElse ErrorForeachNotIterable(containerType, container)
        case _                    => ErrorForeachNotIterable(containerType, container)
      }
      varDecl.id.getType match {
        case TUntyped                      => varDecl.id.setType(expectedVarType)
        case tpe if tpe != expectedVarType => ErrorWrongType(expectedVarType, tpe, varDecl.id)
        case _                             =>
      }
      tcStat(stat)
    case PrintStatTree(expr)               =>
      tcExpr(expr)
      if (expr.getType == TUnit)
        ErrorCantPrintUnitType(expr)
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
        ErrorWrongReturnType(currentMethodSymbol.getType.toString, ret)
      returnStatements += ((ret, TUnit))
    case _: Break | _: Continue            =>
    case expr: ExprTree                    =>
      tcExpr(expr)
  }

  def tcExpr(expr: ExprTree, expected: Type*): Type = tcExpr(expr, expected.toList)

  def tcExpr(expression: ExprTree, expected: Traversable[Type]): Type = {
    val foundType = expression match {
      case lit: Literal[_]                    => lit.getType
      case id: VariableID                     =>
        id.getSymbol.ifInstanceOf[FieldSymbol] { fieldSymbol =>
          checkPrivacy(fieldSymbol.classSymbol, fieldSymbol, id, ErrorFieldPrivacy)
        }
        id.getType
      case id: ClassID                        => id.getType
      case th: This                           => th.getSymbol.getType
      case su: Super                          => su.getSymbol.getType
      case acc: Access                        => tcAccess(acc)
      case assign: Assign                     => tcAssignment(assign)
      case newDecl: New                       => tcNewExpr(newDecl)
      case NewArray(tpe, sizes)               =>
        sizes.foreach(tcExpr(_, Int))
        tpe.getType
      case ArrayLit(expressions)              =>
        val tpes = expressions.map(tcExpr(_))
        val inferredType = getReturnType(tpes)

        // An empty array literal should have Object type
        val tpe = if (inferredType == TUnit) Object else inferredType
        TArray(tpe)
      case And(lhs, rhs)                      =>
        tcExpr(lhs, Bool)
        tcExpr(rhs, Bool)
        Bool
      case Or(lhs, rhs)                       =>
        tcExpr(lhs, Bool)
        tcExpr(rhs, Bool)
        Bool
      case notOp@Not(expr)                    =>
        val tpe = tcExpr(expr)
        if (!tpe.isNullable && tpe != Bool)
          ErrorNotOnNonNullable(notOp)

        Bool
      case eqOp@EqualsOperatorTree(lhs, rhs)  =>
        val lhsTpe = tcExpr(lhs)
        val rhsTpe = tcExpr(rhs)

        if (lhsTpe == TNull || rhsTpe == TNull) {
          if (lhsTpe == TNull && !rhsTpe.isNullable || rhsTpe == TNull && !lhsTpe.isNullable)
            ErrorNonNullableEqualsNull(rhs.getType, eqOp)
          Bool
        } else if (lhsTpe.isInstanceOf[TArray] && rhsTpe.isInstanceOf[TArray]) {
          Bool
        } else {
          tcBinaryOperator(eqOp, lhsTpe, rhsTpe)
        }
      case binOp@BinaryOperatorTree(lhs, rhs) =>
        tcBinaryOperator(binOp, tcExpr(lhs), tcExpr(rhs))
      case incOp@IncrementDecrementTree(obj)  =>
        if (!obj.isInstanceOf[Assignable])
          ErrorInvalidIncrementDecrementExpr(incOp, incOp)

        tcUnaryOperator(incOp, tcExpr(obj))
      case ExtractNullable(expr)              =>
        val exprTpe = tcExpr(expr)
        if (!exprTpe.isNullable)
          ErrorExtractNullableNonNullable(exprTpe, expr)
        exprTpe.getNonNullable
      case unaryOp@UnaryOperatorTree(expr)    =>
        tcUnaryOperator(unaryOp, tcExpr(expr))
      case Is(expr, _)                        =>
        tcExpr(expr)
        Bool
      case As(expr, tpe)                      =>
        tcExpr(expr, Object)
        tpe.getType
      case arrRead@ArrayRead(arr, index)      =>
        val arrTpe = tcExpr(arr)
        arrTpe match {
          case _: TObject     =>
            val indexType = tcExpr(index)
            tcArrayOperator(arrTpe, arrRead, List(indexType), arrTpe, expression)
          case TArray(arrTpe) =>
            tcExpr(index, Int)
            arrTpe
          case tpe            => ErrorWrongType("array", tpe, arr)
        }
      case ArraySlice(arr, start, end, step)  =>
        List(start, end, step).filter(_.isDefined).map(e => tcExpr(e.get, Int))
        tcExpr(arr)
      case Ternary(condition, thn, els)       =>
        tcExpr(condition, Bool)
        val thnType = tcExpr(thn)
        val elsType = tcExpr(els)
        getReturnType(List(thnType, elsType))
      case Elvis(nullableValue, ifNull)       =>
        val nullableTpe = tcExpr(nullableValue)
        if (!nullableTpe.isNullable)
          ErrorElvisOperatorNonNullable(nullableTpe, nullableValue)
        tcExpr(ifNull, nullableTpe)
        nullableTpe.getNonNullable
    }

    def correctType(expectedTpe: Type): Boolean = {
      (expectedTpe == Bool && foundType.isNullable) ||
        foundType.isSubTypeOf(expectedTpe) ||
        expectedTpe.isImplicitlyConvertibleFrom(foundType)
    }

    val res = if (expected.nonEmpty && !expected.exists(correctType))
      ErrorWrongType(expected, foundType, expression)
    else
      foundType

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
          ErrorInstantiateTrait(classSymbol.name, newExpr)
        } else {
          classSymbol.lookupMethod("new", argTypes, importMap) match {
            case Some(constructorSymbol) =>
              checkPrivacy(classSymbol, constructorSymbol, newExpr, ErrorConstructorPrivacy)
              newExpr.setSymbol(constructorSymbol)
            case None if exprs.nonEmpty  =>
              val methodSignature = "new" + exprs.map(_.getType).mkString("(", " , ", ")")
              ErrorDoesntHaveConstructor(tpe.name, methodSignature, newExpr)
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
        objType match {
          case TObject(classSymbol) =>
            classSymbol.lookupMethod(meth.name, argTypes.get, importMap) map { methSymbol =>
              checkPrivacy(classSymbol, methSymbol, app, ErrorMethodPrivacy)
              checkStaticMethodConstraints(acc, classSymbol, methSymbol, app)
              TypeChecking.methodUsage += methSymbol -> true
              inferTypeOfMethod(methSymbol)
              meth.setSymbol(methSymbol)
              meth.getType
            } getOrElse {
              val alternatives = classSymbol.methods.filter(_.argTypes == argTypes.get).map(_.name)
              ErrorClassDoesntHaveMethod(classSymbol.name, methSignature, meth.name, alternatives, app)
            }
          case _: TArray            =>
            if (args.nonEmpty || meth.name != "Size")
              ErrorMethodOnWrongType(methSignature, objType.toString, app)
            meth.setSymbol(new MethodSymbol("Size", new ClassSymbol("Array", false), None, Set()).setType(Int))
            Int
          case _                    => TError
        }
      case fieldId@VariableID(fieldName) =>
        objType match {
          case TObject(classSymbol) =>
            classSymbol.lookupField(fieldName) map { fieldSymbol =>
              checkPrivacy(classSymbol, fieldSymbol, app, ErrorFieldPrivacy)
              checkStaticFieldConstraints(acc, classSymbol, fieldSymbol, app)
              fieldId.setSymbol(fieldSymbol)
              fieldId.getType
            } getOrElse {
              val alternatives = classSymbol.fields.keys.toList
              ErrorClassDoesntHaveField(classSymbol.name, fieldName, alternatives, app)
            }
          case _                    => ErrorFieldOnWrongType(objType.toString, app)
        }
    }


    val t = acc match {
      case _: SafeAccess   =>
        if (!objType.isNullable)
          ErrorSafeAccessOnNonNullable(objType, acc)
        tpe.getNullable
      case _: NormalAccess => tpe
    }
    app.setType(t)
    t
  }


  def getReturnType(tpes: Traversable[Type]): Type = {
    var uniqueTpes = tpes.toSet
    if (uniqueTpes.isEmpty)
      return TUnit

    if (uniqueTpes.size == 1)
      return uniqueTpes.head

    val containsNull = uniqueTpes.contains(TNull)
    uniqueTpes = uniqueTpes.filter(_ != TNull)
    val tpe =
      if (uniqueTpes.size == 1) {
        uniqueTpes.head
      } else if (uniqueTpes.exists(_ in Primitives)) {
        // More than one type and at least one is a primitive
        Object
      } else {
        val s = uniqueTpes.head.getSuperTypes
        val commonTypes = uniqueTpes.drop(1).foldLeft(s)((common, tpe) => common.intersect(tpe.getSuperTypes))
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
            thisSymbol.lookupParentMethod(meth.name, argTypes.get, importMap) map {
              _.classSymbol
            } getOrElse {
              return ErrorNoSuperTypeHasMethod(thisSymbol.name, methSignature, app)
            }
          case Identifier(fieldName) =>
            thisSymbol.lookupParentField(fieldName).map {
              _.classSymbol
            } getOrElse {
              return ErrorNoSuperTypeHasField(thisSymbol.name, fieldName, app)
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

    to match {
      case _: VariableID          =>
        val toTpe = tcExpr(to)

        tcExpr(expr, toTpe)
        toTpe
      case Access(_, application) =>
        application match {
          case _: MethodCall => ErrorAssignValueToMethodCall(assignment)
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
          case tpe              => ErrorWrongType(arr.getType + "[]", tpe, arr)
        }
    }
  }

  def tcBinaryOperator(operator: OperatorTree, arg1: Type, arg2: Type): Type = {
    val argList = List(arg1, arg2)
    typeCheckOperator(arg1, operator, argList)
      .orElse(typeCheckOperator(arg2, operator, argList))
      .getOrElse(ErrorOverloadedOperatorNotFound(operator, argList, operator))
  }

  def tcUnaryOperator(expr: OperatorTree, arg: Type): Type = {
    val argList = List(arg)
    typeCheckOperator(arg, expr, argList)
      .getOrElse(ErrorOverloadedOperatorNotFound(expr, argList, expr))
  }

  def tcArrayOperator(classTpe: Type, opType: ArrayOperatorTree, argList: List[Type], arrTpe: Type, pos: Positioned): Type = {
    typeCheckOperator(classTpe, opType, argList)
      .getOrElse(ErrorIndexingOperatorNotFound(opType, argList, arrTpe.toString, pos))
  }

  def checkMethodUsage(): Unit = {
    // Check method usage
    // TODO: Refactoring of typechecker global variables etc.
    methodUsage
      .filter { case (_, used) => !used }
      .foreach { case (method, _) => WarningUnusedPrivateMethod(method.signature, method) }
    methodUsage = Map[MethodSymbol, Boolean]()
  }

  def checkCorrectOverrideReturnTypes(cu: CompilationUnit): Unit =
    cu.classes.foreach {
      clazz =>
        clazz.methods.foreach {
          meth =>
            val classSymbol = clazz.getSymbol
            val methSymbol = meth.getSymbol
            val methType = meth.getSymbol.getType
            classSymbol.lookupParentMethod(methSymbol.name, methSymbol.argTypes, importMap)
              .filter { parentMeth =>
                parentMeth.getType != methType && !methType.isSubTypeOf(parentMeth.getType)
              }
              .foreach { parentMeth =>
                ErrorOverridingMethodDifferentReturnType(meth.getSymbol.signature,
                  classSymbol.name,
                  meth.getSymbol.getType.toString,
                  parentMeth.classSymbol.name,
                  parentMeth.getType.toString,
                  meth
                )
              }
        }
    }


  def checkTraitsAreImplemented(cu: CompilationUnit): Unit =
    cu.classes.filterInstance[ClassDecl].foreach { classDecl =>
      classDecl.traits.foreach(t => checkTraitIsImplemented(classDecl, t.getSymbol))
    }

  private def typeCheckOperator(classType: Type, operator: OperatorTree, args: List[Type]): Option[Type] =
    operator.lookupOperator(classType, args, importMap).map { operatorSymbol =>
      val classSymbol = classType.asInstanceOf[TObject].classSymbol
      checkPrivacy(classSymbol, operatorSymbol, operator, ErrorOperatorPrivacy)
      inferTypeOfMethod(operatorSymbol)
      operator.setType(operatorSymbol.getType)
      operatorSymbol.getType
    }

  private def checkTraitIsImplemented(classDecl: IDClassDeclTree, implementedTrait: ClassSymbol) =
    implementedTrait.abstractMethods()
      .filter { case (method, _) => !classDecl.getSymbol.implementsMethod(method) }
      .foreach { case (method, owningTrait) =>
        ErrorUnimplementedMethodFromTrait(classDecl.id.name,
          method.signature,
          owningTrait.name, classDecl.id)
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
    classSymbol.lookupMethod("Iterator", Nil, importMap).flatMap { methSymbol =>
      inferTypeOfMethod(methSymbol) match {
        case TObject(methodClassSymbol) =>
          methodClassSymbol.lookupMethod("HasNext", Nil, importMap)
            .filter {inferTypeOfMethod(_) == Bool}
            .flatMap { _ =>
              methodClassSymbol.lookupMethod("Next", Nil, importMap).map(inferTypeOfMethod)
            }
        case _                          => None
      }
    }

  private def inferTypeOfMethod(methodSymbol: MethodSymbol): Type = {
    if (methodSymbol.getType == TUntyped)
      new TypeChecker(ctx, importMap, methodSymbol, currentMethodSymbol :: methodStack).tcMethod()
    methodSymbol.getType
  }

  private def checkStaticMethodConstraints(acc: Access, classSymbol: ClassSymbol, methodSymbol: MethodSymbol, pos: Positioned) = {
    if (!methodSymbol.isStatic && acc.isStatic)
      ErrorNonStaticMethodAsStatic(methodSymbol.name, pos)

    if (acc.obj.isInstanceOf[This] && currentMethodSymbol.isStatic && !methodSymbol.isStatic)
      ErrorNonStaticMethodFromStatic(methodSymbol.name, pos)
  }

  private def checkStaticFieldConstraints(acc: Access, classSymbol: ClassSymbol, varSymbol: FieldSymbol, pos: Positioned) = {
    if (!varSymbol.isStatic && acc.isStatic)
      ErrorNonStaticFieldAsStatic(varSymbol.name, pos)

    if (acc.obj.isInstanceOf[This] && currentMethodSymbol.isStatic && !varSymbol.isStatic)
      ErrorNonStaticFieldFromStatic(varSymbol.name, pos)
  }

  private def checkPrivacy[Sym <: Symbol with Modifiable](
    classSymbol: ClassSymbol,
    sym: Sym,
    pos: Positioned,
    error: (Sym, String, String, Positioned) => Type) = {
    if (!isValidAccess(classSymbol, sym.accessability))
      error(sym, classSymbol.name, currentMethodSymbol.classSymbol.name, pos)
  }

  private def isValidAccess(classSymbol: ClassSymbol, access: Accessability) = access match {
    case Public()                                                                                => true
    case Private() if classSymbol == currentMethodSymbol.classSymbol                             => true
    case Protected() if currentMethodSymbol.classSymbol.getType.isSubTypeOf(classSymbol.getType) => true
    case _                                                                                       => false
  }
}