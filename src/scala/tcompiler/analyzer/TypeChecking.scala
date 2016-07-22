package tcompiler
package analyzer

import tcompiler.analyzer.Symbols._
import tcompiler.analyzer.Types._
import tcompiler.ast.Printer
import tcompiler.ast.Trees._
import tcompiler.imports.ImportMap
import tcompiler.utils.Extensions._
import tcompiler.utils._

import scala.collection.TraversableLike
import scala.collection.mutable.ArrayBuffer

object TypeChecking extends Pipeline[List[CompilationUnit], List[CompilationUnit]] {

  val hasBeenTypechecked = scala.collection.mutable.Set[MethodSymbol]()
  var methodUsage        = Map[MethodSymbol, Boolean]()


  /**
    * Typechecking does not produce a value, but has the side effect of
    * attaching types to trees and potentially outputting error messages.
    */
  def run(ctx: Context)(cus: List[CompilationUnit]): List[CompilationUnit] = {
    cus foreach { cu =>
      // Typecheck fields
      cu.classes.foreach { classDecl =>
        val typeChecker = new TypeChecker(ctx, cu.importMap, new MethodSymbol("", classDecl.getSymbol, None, Set()))
        classDecl.fields.foreach(typeChecker.tcStat(_))
      }
    }

    cus foreach { cu =>
      // Typecheck methods
      cu.classes.foreach { classDecl =>
        classDecl.methods.foreach { method =>
          val methodSymbol = method.getSymbol
          if (!methodUsage.contains(methodSymbol))
            methodUsage += methodSymbol -> !method.accessability.isInstanceOf[Private]
          new TypeChecker(ctx, cu.importMap, methodSymbol).tcMethod()
        }
      }

      val c = new ClassSymbol("", false)
      val tc = new TypeChecker(ctx, cu.importMap, new MethodSymbol("", c, None, Set()))

      tc.checkMethodUsage()
      tc.checkCorrectOverrideReturnTypes(cu)
      tc.checkTraitsAreImplemented(cu)
    }
    cus
  }
}

class TypeChecker(override var ctx: Context,
                  override var importMap: ImportMap,
                  currentMethodSymbol: MethodSymbol,
                  methodStack: List[MethodSymbol] = List()) extends TypeCheckingErrors {

  import TypeChecking._

  val returnStatements = ArrayBuffer[(Return, Type)]()

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
      returnStatements.map(_._1).foreach {
        _.setType(methType)
      }
      checkOperatorType(methType)
      return
    }

    if (returnStatements.isEmpty) {
      currentMethodSymbol.setType(TUnit)
      return
    }

    val returnTypes = returnStatements.map(_._2)
    val inferredType = getReturnType(returnTypes)
    returnStatements.map(_._1).foreach {
      _.setType(inferredType)
    }

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
        ErrorOperatorWrongReturnType(op.operatorString, correctOperatorType.toString, tpe.toString, op)
    case _                  =>
  }

  def tcStat(statement: StatTree): Unit = statement match {
    case Block(stats)                              =>
      stats.foreach(tcStat)
    case varDecl@VarDecl(tpe, id, init, modifiers) =>
      tpe match {
        case Some(t) => init match {
          case Some(expr) => tcExpr(expr, t.getType)
          case _          =>
        }
        case None    => init match {
          case Some(expr) =>
            val inferedType = tcExpr(expr)
            id.setType(inferedType)
          case _          => ErrorNoTypeNoInitalizer(varDecl.id.name, varDecl)
        }
      }
    case If(expr, thn, els)                        =>
      tcExpr(expr, Bool)
      tcStat(thn)
      if (els.isDefined)
        tcStat(els.get)
    case While(expr, stat)                         =>
      tcExpr(expr, Bool)
      tcStat(stat)
    case For(init, condition, post, stat)          =>
      init.foreach(tcStat(_))
      tcExpr(condition, Bool)
      post.foreach(tcStat)
      tcStat(stat)
    case Foreach(varDecl, container, stat)         =>
      val containerType = tcExpr(container)
      val expectedVarType = containerType match {
        case TArray(arrTpe)       =>
          arrTpe
        case TObject(classSymbol) =>
          getIteratorType(classSymbol) match {
            case Some(t) => t
            case None    => ErrorForeachNotIterable(containerType, container)
          }
        case _                    => ErrorForeachNotIterable(containerType, container)
      }
      varDecl.id.getType match {
        case TUntyped => varDecl.id.setType(expectedVarType)
        case tpe      =>
          if (tpe != expectedVarType)
            ErrorWrongType(expectedVarType, tpe, varDecl.id)
      }
      tcStat(stat)
    case PrintStatTree(expr)                       =>
      tcExpr(expr)
      if (expr.getType == TUnit)
        ErrorCantPrintUnitType(expr)
    case Error(expr)                               =>
      tcExpr(expr, String)
    case ret@Return(Some(expr))                    =>
      val t = currentMethodSymbol.getType match {
        case TUntyped => tcExpr(expr)
        case retType  => tcExpr(expr, retType)
      }
      returnStatements += ((ret, t))
    case ret@Return(None)                          =>
      if (currentMethodSymbol.getType != TUntyped && currentMethodSymbol.getType != TUnit)
        ErrorWrongReturnType(currentMethodSymbol.getType.toString, ret)
      returnStatements += ((ret, TUnit))
    case _: Break | _: Continue                    =>
    case expr: ExprTree                            =>
      tcExpr(expr)
  }

  def tcExpr(expr: ExprTree, expected: Type*): Type = tcExpr(expr, expected.toList)

  def tcExpr(expression: ExprTree, expected: Traversable[Type]): Type = {
    val foundType = expression match {
      case lit: Literal[_]                               => lit.getType
      case id: VariableID                                =>
        id.getSymbol match {
          case sym: FieldSymbol => checkFieldPrivacy(sym.classSymbol, sym, id)
          case _                =>
        }
        id.getType
      case id: ClassID                                   => id.getType
      case th: This                                      => th.getSymbol.getType
      case su: Super                                     => su.getSymbol.getType
      case acc: Access                                   => tcAccess(acc)
      case assign: Assign                                => tcAssignment(assign)
      case newArray@NewArray(tpe, sizes)                 =>
        sizes.foreach(tcExpr(_, Int))
        var arrayType = tpe.getType
        for (i <- 1 to newArray.dimension)
          arrayType = TArray(arrayType)
        arrayType
      case ArrayLit(expressions)                         =>
        val tpes = expressions.map(tcExpr(_))
        val arrTpe = getReturnType(tpes)
        val a = if (arrTpe == TUnit) Object else arrTpe
        TArray(a)
      case arithmeticOp@ArithmeticOperatorTree(lhs, rhs) =>
        val args = (tcExpr(lhs), tcExpr(rhs))
        args match {
          case _ if args.anyIs(Object)      => tcBinaryOperator(arithmeticOp, args)
          case _ if args.anyIs(Bool, Array) => ErrorOperatorDoesNotExist(arithmeticOp, args, expression)
          case _ if args.anyIs(Double)      => Double
          case _ if args.anyIs(Float)       => Float
          case _ if args.anyIs(Long)        => Long
          case _                            => Int
        }
      case logicalOp@LogicalOperatorTree(lhs, rhs)       =>
        val args = (tcExpr(lhs), tcExpr(rhs))
        args match {
          case _ if args.anyIs(Object)                     => tcBinaryOperator(logicalOp, args)
          case _ if args.bothAre(Bool)                     => Bool
          case _ if args.anyIs(Bool, Float, Double, Array) => ErrorOperatorDoesNotExist(logicalOp, args, expression)
          case _ if args.anyIs(Long)                       => Long
          case _ if args.anyIs(Int)                        => Int
          case _ if args.bothAre(Char)                     => Int
          case _                                           => ErrorOperatorDoesNotExist(logicalOp, args, expression)
        }
      case shiftOp@ShiftOperatorTree(lhs, rhs)           =>
        val args = (tcExpr(lhs), tcExpr(rhs))
        args match {
          case _ if args.anyIs(Object)                     => tcBinaryOperator(shiftOp, args)
          case _ if args.anyIs(Bool, Float, Double, Array) => ErrorOperatorDoesNotExist(shiftOp, args, expression)
          case _ if args.anyIs(Long)                       => Long
          case _                                           => Int
        }
      case compOp@ComparisonOperatorTree(lhs, rhs)       =>
        val args = (tcExpr(lhs), tcExpr(rhs))
        args match {
          case _ if args.anyIs(Object)      => tcBinaryOperator(compOp, args, Some(Bool))
          case _ if args.anyIs(Bool, Array) => ErrorOperatorDoesNotExist(compOp, args, expression)
          case _                            => Bool
        }
      case eqOp@EqualsOperatorTree(lhs, rhs)             =>
        val args = (tcExpr(lhs), tcExpr(rhs))
        args match {
          case _ if args.anyIs(Object)                        =>
            // TODO: Compare java object by reference
            tcBinaryOperator(eqOp, args)
          case (x, y) if x.isSubTypeOf(y) || y.isSubTypeOf(x) => // Valid
          case _ if args.anyIs(Bool, Array)                   => ErrorOperatorDoesNotExist(eqOp, args, expression)
          case _                                              =>
        }
        Bool
      case And(lhs, rhs)                                 =>
        tcExpr(lhs, Bool)
        tcExpr(rhs, Bool)
        Bool
      case Or(lhs, rhs)                                  =>
        tcExpr(lhs, Bool)
        tcExpr(rhs, Bool)
        Bool
      case notOp@Not(expr)                               =>
        tcExpr(expr, Bool, Types.Object) match {
          case obj: TObject => tcUnaryOperator(notOp, obj, Some(Bool))
          case _            =>
        }
        Bool
      case Is(expr, tpe)                                 =>
        tcExpr(expr, Object)
        Bool
      case As(expr, tpe)                                 =>
        tcExpr(expr, Object)
        tpe.getType
      case arrRead@ArrayRead(arr, index)                 =>
        val arrTpe = tcExpr(arr)
        arrTpe match {
          case TObject(classSymbol) =>
            val indexType = tcExpr(index)
            val argList = List(indexType)
            tcArrayOperator(arrTpe, arrRead, argList, arrTpe, expression)
          case TArray(arrTpe)       =>
            tcExpr(index, Int)
            arrTpe
          case tpe                  => ErrorWrongType("array", tpe, arr)
        }
      case ArraySlice(arr, start, end)                   =>
        start.ifDefined(tcExpr(_, Int))
        end.ifDefined(tcExpr(_, Int))
        tcExpr(arr)
      case newDecl@New(tpe, exprs)                       =>
        val argTypes = exprs.map(tcExpr(_))
        tpe.getType match {
          case TObject(classSymbol) =>
            if (classSymbol.isAbstract) {
              ErrorInstantiateTrait(classSymbol.name, newDecl)
            } else {
              classSymbol.lookupMethod("new", argTypes) match {
                case Some(constructorSymbol) => checkConstructorPrivacy(classSymbol, constructorSymbol, newDecl)
                case None if exprs.nonEmpty  =>
                  val methodSignature = tpe.name + exprs.map(_.getType).mkString("(", " , ", ")")
                  ErrorDoesntHaveConstructor(tpe.name, methodSignature, newDecl)
                case _                       =>
              }
            }
          case primitiveType        =>
            if (exprs.size > 1) {
              ErrorNewPrimitive(tpe.name, argTypes, newDecl)
            } else if (exprs.size == 1) {
              val arg = exprs.head.getType
              if (!primitiveType.isImplicitlyConvertableFrom(arg))
                ErrorNewPrimitive(tpe.name, argTypes, newDecl)
            }
        }
        tpe.getType
      case negOp@Negation(expr)                          =>
        tcExpr(expr, Object, Int, Char, Long, Double, Float) match {
          case x: TObject => tcUnaryOperator(negOp, x)
          case _: TChar   => Int // Negation of char is int
          case x          => x
        }
      case hashOp@Hash(expr)                             =>
        val exprType = tcExpr(expr)
        exprType match {
          case obj: TObject => tcUnaryOperator(hashOp, obj, Some(Int))
          case _            =>
        }
        Int
      case incOp@IncrementDecrementTree(obj)             =>
        obj match {
          case id: VariableID           => checkReassignment(id, expression)
          case _: Access | _: ArrayRead =>
          case _                        => ErrorInvalidIncrementDecrementExpr(incOp, obj)
        }
        // TODO: Allow increment decrement for Bool types?
        tcExpr(obj, Object, Int, Char, Long, Double, Float) match {
          case x: TObject => tcUnaryOperator(incOp, x, Some(x)) // Requires same return type as type
          case x          => x
        }
      case notOp@LogicNot(expr)                          =>
        tcExpr(expr, Object, Int, Long, Char) match {
          case x: TObject => tcUnaryOperator(notOp, x)
          case _: TLong   => Long
          case _          => Int
        }
      case Ternary(condition, thn, els)                  =>
        tcExpr(condition, Bool)
        val thnType = tcExpr(thn)
        val elsType = tcExpr(els)
        getReturnType(List(thnType, elsType))
      case Elvis(nullableValue, ifNull)                  =>
        val nullableTpe = tcExpr(nullableValue)
        if (!nullableTpe.isNullable)
          ErrorElvisOperatorNonNullable(nullableTpe, nullableValue)
        tcExpr(ifNull, nullableTpe)
        nullableTpe.getNonNullable
      case ExtractNullable(expr)                         =>
        val exprTpe = tcExpr(expr)
        if (!exprTpe.isNullable)
          ErrorExtractNullableNonNullable(exprTpe, expr)
        exprTpe.getNonNullable
    }

    def correctType(expectedTpe: Type) =
      foundType.isSubTypeOf(expectedTpe) || expectedTpe.isImplicitlyConvertableFrom(foundType)

    val res =
      if (expected.nonEmpty && !expected.exists(correctType))
        ErrorWrongType(expected, foundType, expression)
      else
        foundType

    expression.setType(res)
    res
  }

  def tcAccess(acc: Access): Type = {
    val app = acc.application

    // If it's a method access we type check it now
    val argTypes = app match {
      case MethodCall(meth, args) => Some(args.map(tcExpr(_)))
      case _                      => None
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
            classSymbol.lookupMethod(meth.name, argTypes.get) match {
              case Some(methSymbol) =>
                checkMethodPrivacy(classSymbol, methSymbol, app)
                checkStaticMethodConstraints(acc, classSymbol, methSymbol, app)

                TypeChecking.methodUsage += methSymbol -> true
                inferTypeOfMethod(methSymbol)
                meth.setSymbol(methSymbol)
                meth.getType
              case None             =>
                ErrorClassDoesntHaveMethod(classSymbol.name, methSignature, app)
            }
          case TArray(arrTpe)       =>
            if (args.nonEmpty || meth.name != "Size")
              ErrorMethodOnWrongType(methSignature, objType.toString, app)
            meth.setSymbol(new MethodSymbol("Size", new ClassSymbol("Array", false), None, Set()).setType(Int))
            Int
          case _                    => ErrorMethodOnWrongType(methSignature, objType.toString, app)
        }
      case fieldId@VariableID(fieldName) =>
        objType match {
          case TObject(classSymbol) =>
            classSymbol.lookupField(fieldName) match {
              case Some(varSymbol) =>
                checkFieldPrivacy(classSymbol, varSymbol, app)
                checkStaticFieldConstraints(acc, classSymbol, varSymbol, app)
                fieldId.setSymbol(varSymbol)
                fieldId.getType
              case None            =>
                ErrorClassDoesntHaveField(classSymbol.name, fieldName, app)
            }
          case _                    => ErrorFieldOnWrongType(objType.toString, app)
        }
    }


    val t = acc match {
      case sa: SafeAccess  =>
        if (!objType.isNullable)
          ErrorSafeAccessOnNonNullable(objType, sa)
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
    println(containsNull)
    val tpe =
      if (uniqueTpes.size == 1) {
        uniqueTpes.head
      } else if (uniqueTpes.exists(_.isInstanceOf[PrimitiveType])) {
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
          case MethodCall(meth, args) =>
            thisSymbol.lookupParentMethod(meth.name, argTypes.get) match {
              case Some(methodSymbol) => methodSymbol.classSymbol
              case None               => return ErrorNoSuperTypeHasMethod(thisSymbol.name, methSignature, app)
            }
          case Identifier(fieldName)  =>
            thisSymbol.lookupParentField(fieldName) match {
              case Some(fieldSymbol) => fieldSymbol.classSymbol
              case None              => return ErrorNoSuperTypeHasField(thisSymbol.name, fieldName, app)
            }
          case _                      => ???
        }
        sup.setSymbol(classSymbol)
        sup.setType(classSymbol.getType)
        classSymbol.getType
      case _                                                 => tcExpr(obj)
    }
  }

  def tcAssignment(assignment: Assign): Type = {
    val to = assignment.to
    val expr = assignment.expr

    to match {
      case id: VariableID           =>
        val toTpe = tcExpr(to)

        checkReassignment(id, assignment)
        tcExpr(expr, toTpe)
        toTpe
      case Access(obj, application) =>
        application match {
          case _: MethodCall  => ErrorAssignValueToMethodCall(assignment)
          case id: VariableID =>
            val toTpe = tcExpr(to)
            checkReassignment(id, assignment)
            tcExpr(expr, toTpe)
            toTpe
          case _              => ???
        }
      case ArrayRead(arr, index)    =>
        val arrTpe = tcExpr(arr)
        to.setType(arr)
        arrTpe match {
          case TObject(classSymbol) =>
            val indexType = tcExpr(index)
            val exprType = tcExpr(expr)

            val argList = List(indexType, exprType)
            tcArrayOperator(arrTpe, assignment, argList, arrTpe, assignment)
            exprType
          case TArray(arrayTpe)     =>
            tcExpr(index, Int)
            tcExpr(expr, arrayTpe)
            arrTpe.asInstanceOf[TArray].tpe
          case tpe                  => ErrorWrongType(arr.getType + "[]", tpe, arr)
        }
    }
  }

  def tcBinaryOperator(operator: OperatorTree, args: (Type, Type), expectedType: Option[Type] = None): Type = {
    val argList = List(args._1, args._2)
    typeCheckOperator(args._1, operator, argList)
    .orElse(typeCheckOperator(args._2, operator, argList))
    .getOrElse(ErrorOverloadedOperatorNotFound(operator, argList, operator))
  }

  def tcUnaryOperator(expr: OperatorTree, arg: Type, expectedType: Option[Type] = None): Type = {
    val argList = List(arg)
    typeCheckOperator(arg, expr, argList)
    .getOrElse(ErrorOverloadedOperatorNotFound(expr, argList, expr))
  }

  def tcArrayOperator(classTpe: Type, opType: ArrayOperatorTree, argList: List[Type], arrTpe: Type, pos: Positioned) = {
    typeCheckOperator(classTpe, opType, argList)
    .getOrElse(ErrorIndexingOperatorNotFound(opType, argList, arrTpe.toString, pos))
  }

  private def typeCheckOperator(classType: Type, operator: OperatorTree, args: List[Type]): Option[Type] = {
    operator.lookupOperator(classType, args) match {
      case Some(operatorSymbol) =>
        val classSymbol = classType.asInstanceOf[TObject].classSymbol
        checkOperatorPrivacy(classSymbol, operatorSymbol, operator)
        inferTypeOfMethod(operatorSymbol)
        operator.setType(operatorSymbol.getType)
        Some(operatorSymbol.getType)
      case None                 => None
    }
  }


  def checkMethodUsage() = {
    // Check method usage
    // TODO: Refactoring of typechecker global variables etc.
    methodUsage foreach {
      case (method, used) =>
        if (!used)
          WarningUnusedPrivateField(method.signature, method)
    }
    methodUsage = Map[MethodSymbol, Boolean]()
  }

  def checkCorrectOverrideReturnTypes(cu: CompilationUnit) =
    cu.classes.foreach { clazz =>
      clazz.methods.foreach { meth =>
        val classSymbol = clazz.getSymbol
        val methSymbol = meth.getSymbol

        classSymbol.lookupParentMethod(methSymbol.name, methSymbol.argTypes) match {
          case Some(parentMeth) if parentMeth.getType != meth.getSymbol.getType =>
            val parentType = parentMeth.getType
            val tpe = meth.getSymbol.getType
            if (!tpe.isSubTypeOf(parentType)) {
              ErrorOverridingMethodDifferentReturnType(meth.getSymbol.signature,
                                                        classSymbol.name,
                                                        meth.getSymbol.getType.toString,
                                                        parentMeth.classSymbol.name,
                                                        parentMeth.getType.toString,
                                                        meth)
            }
          case _                                                                =>
        }
      }
    }


  def checkTraitsAreImplemented(cu: CompilationUnit) =
    cu.classes.filter(!_.isAbstract).foreach { classDecl =>
      classDecl.implementedTraits.foreach(t => traitIsImplemented(classDecl, t.getSymbol))
    }

  private def checkReassignment(id: VariableID, pos: Positioned) =
    if (id.hasSymbol && id.getSymbol.isFinal)
      ErrorReassignmentToVal(id.name, pos)

  private def traitIsImplemented(classDecl: ClassDecl, implementedTrait: ClassSymbol) = {
    val unimplementedMethods = implementedTrait.unimplementedMethods()
    unimplementedMethods.foreach { case (method, owningTrait) =>
      if (!classDecl.getSymbol.implementsMethod(method))
        ErrorUnimplementedMethodFromTrait(classDecl.id.name,
                                           method.signature,
                                           owningTrait.name, classDecl.id)
    }
  }

  /**
    * This is hardcoded and does not depend on the trait Iterable.
    * This allows for classes which do not implement the Itreable trait
    * but which does provide an Iterator method which returns an Iterator
    * with the methods HasNext and Next of correct types to still be
    * applicable for ForEach loops.
    */
  private def getIteratorType(classSymbol: ClassSymbol): Option[Type] = {
    classSymbol.lookupMethod("Iterator", List()) match {
      case Some(methSymbol) =>
        inferTypeOfMethod(methSymbol)
        methSymbol.getType match {
          case TObject(methodClassSymbol) =>
            methodClassSymbol.lookupMethod("HasNext", List()) match {
              case Some(hasNextMethod) =>
                inferTypeOfMethod(hasNextMethod)
                if (hasNextMethod.getType != Bool)
                  return None
              case None                => return None
            }
            methodClassSymbol.lookupMethod("Next", List()) match {
              case Some(nextMethod) =>
                inferTypeOfMethod(nextMethod)
                return Some(nextMethod.getType)
              case None             =>
            }
          case _                          =>
        }
      case None             =>
    }
    None
  }

  private def inferTypeOfMethod(methodSymbol: MethodSymbol) = {
    if (methodSymbol.getType == TUntyped)
      new TypeChecker(ctx, importMap, methodSymbol, currentMethodSymbol :: methodStack).tcMethod()
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

  private def checkConstructorPrivacy(classSymbol: ClassSymbol, methodSymbol: MethodSymbol, pos: Positioned): Unit =
    if (!checkPrivacy(classSymbol, methodSymbol))
      ErrorConstructorPrivacy(methodSymbol, classSymbol.name, currentMethodSymbol.classSymbol.name, pos)

  private def checkMethodPrivacy(classSymbol: ClassSymbol, methodSymbol: MethodSymbol, pos: Positioned): Unit =
    if (!checkPrivacy(classSymbol, methodSymbol))
      ErrorMethodPrivacy(methodSymbol, classSymbol.name, currentMethodSymbol.classSymbol.name, pos)

  private def checkFieldPrivacy(classSymbol: ClassSymbol, varSymbol: FieldSymbol, pos: Positioned): Unit =
    if (!checkPrivacy(classSymbol, varSymbol))
      ErrorFieldPrivacy(varSymbol, classSymbol.name, currentMethodSymbol.classSymbol.name, pos)

  private def checkOperatorPrivacy(classSymbol: ClassSymbol, opSymbol: OperatorSymbol, pos: Positioned): Unit =
    if (!checkPrivacy(classSymbol, opSymbol))
      ErrorOperatorPrivacy(opSymbol, classSymbol.name, currentMethodSymbol.classSymbol.name, pos)

  private def checkPrivacy(classSymbol: ClassSymbol, m: Modifiable) = m.accessability match {
    case Public()                                                                                => true
    case Private() if classSymbol == currentMethodSymbol.classSymbol                             => true
    case Protected() if currentMethodSymbol.classSymbol.getType.isSubTypeOf(classSymbol.getType) => true
    case _                                                                                       => false
  }

}
