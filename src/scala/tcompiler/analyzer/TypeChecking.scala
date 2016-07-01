package tcompiler
package analyzer

import tcompiler.analyzer.Symbols._
import tcompiler.analyzer.Types._
import tcompiler.ast.Trees._
import tcompiler.utils.Extensions._
import tcompiler.utils._

import scala.collection.mutable.ArrayBuffer

object TypeChecking extends Pipeline[List[Program], List[Program]] {

  val hasBeenTypechecked = scala.collection.mutable.Set[MethodSymbol]()
  var methodUsage        = Map[MethodSymbol, Boolean]()


  /**
    * Typechecking does not produce a value, but has the side effect of
    * attaching types to trees and potentially outputting error messages.
    */
  def run(ctx: Context)(progs: List[Program]): List[Program] = {
    progs foreach { prog =>
      // Typecheck fields
      prog.classes.foreach { classDecl =>
        val typeChecker = new TypeChecker(ctx, new MethodSymbol("", classDecl.getSymbol, None, Set()))
        classDecl.fields.foreach(typeChecker.tcStat(_))
      }
    }

    progs foreach { prog =>
      // Typecheck methods
      prog.classes.foreach { classDecl =>
        classDecl.methods.foreach { method =>
          val methodSymbol = method.getSymbol
          if (!methodUsage.contains(methodSymbol))
            methodUsage += methodSymbol -> !method.accessability.isInstanceOf[Private]
          new TypeChecker(ctx, methodSymbol).tcMethod()
        }
      }

      val c = new ClassSymbol("", false)
      val tc = new TypeChecker(ctx, new MethodSymbol("", c, None, Set()))

      tc.checkMethodUsage()
      tc.checkCorrectOverrideReturnTypes(prog)
      tc.checkTraitsAreImplemented(prog)
    }
    progs
  }
}

class TypeChecker(
  override var ctx: Context,
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

    if (currentMethodSymbol.getType != TUntyped) {
      currentMethodSymbol.setType(tcOperator(currentMethodSymbol.getType))
      return
    }

    if (currentMethodSymbol.getType != TUntyped)
      return

    if (returnStatements.isEmpty) {
      currentMethodSymbol.setType(TUnit)
      return
    }

    if (returnStatements.map(_._2).toSet.size > 1) {
      val s = returnStatements.map { case (stat, tpe) => s"Line ${stat.line} -> '$tpe'" }.mkString(", ")
      ErrorMultipleReturnTypes(s, returnStatements.head._1)
      currentMethodSymbol.setType(TError)
      return
    }

    val inferredType = returnStatements.head._2


    currentMethodSymbol.setType(tcOperator(inferredType))
  }

  def tcOperator(tpe: Type) = currentMethodSymbol match {
    case op: OperatorSymbol =>
      // Special rules for some operators
      val correctOperatorType = getCorrectOperatorType(op)
      correctOperatorType match {
        case Some(correctType) if tpe != correctType =>
          ErrorOperatorWrongReturnType(op.operatorString, correctType.toString, tpe.toString, op)
        case _                                       => tpe
      }
    case _                  => tpe
  }

  private def getCorrectOperatorType(op: OperatorSymbol) = {
    op.operatorType match {
      case _: ArrayAssign               => Some(TUnit)
      case _: Hash                      => Some(TInt)
      case ComparisonOperatorTree(_, _) => Some(TBool)
      case EqualsOperatorTree(_, _)     => Some(TBool)
      case _                            => None
    }
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
      tcExpr(expr, TBool)
      tcStat(thn)
      if (els.isDefined)
        tcStat(els.get)
    case While(expr, stat)                         =>
      tcExpr(expr, TBool)
      tcStat(stat)
    case For(init, condition, post, stat)          =>
      init.foreach(tcStat(_))
      tcExpr(condition, TBool)
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

  def tcExpr(expression: ExprTree, expected: List[Type]): Type = {
    val foundType = expression match {
      case _: StringLit                  => String
      case _: IntLit                     => TInt
      case _: LongLit                    => TLong
      case _: FloatLit                   => TFloat
      case _: DoubleLit                  => TDouble
      case _: CharLit                    => TChar
      case _: True                       => TBool
      case _: False                      => TBool
      case _: Null                       => TNull
      case id: VariableID                =>
        id.getSymbol match {
          case sym: FieldSymbol => checkFieldPrivacy(sym.classSymbol, sym, id)
          case _                =>
        }
        id.getType
      case id: ClassID                   => id.getType
      case th: This                      => th.getSymbol.getType
      case su: Super                     => su.getSymbol.getType
      case acc: Access                   => tcAccess(acc)
      case assign: Assign                => tcAssignment(assign)
      case newArray@NewArray(tpe, sizes) =>
        sizes.foreach(tcExpr(_, TInt))
        var arrayType = tpe.getType
        for (i <- 1 to newArray.dimension)
          arrayType = TArray(arrayType)
        arrayType
      case ArrayLit(expressions)                         =>
        val tpes = expressions.map(tcExpr(_))
        if (tpes.isEmpty) {
          TArray(Types.Object)
        } else {
          val typeSet = tpes.toSet
          if (typeSet.size > 1)
            ErrorMultipleArrayLitTypes(typeSet.map(t => s"'$t'").mkString(", "), expression)
          TArray(tpes.head)
        }
      case arithmeticOp@ArithmeticOperatorTree(lhs, rhs) =>
        val args = (tcExpr(lhs), tcExpr(rhs))
        args match {
          case _ if args.anyIs(Object)       => tcBinaryOperator(arithmeticOp, args)
          case _ if args.anyIs(TBool, Array) => ErrorOperatorDoesNotExist(arithmeticOp, args, expression)
          case _ if args.anyIs(TDouble)      => TDouble
          case _ if args.anyIs(TFloat)       => TFloat
          case _ if args.anyIs(TLong)        => TLong
          case _                             => TInt
        }
      case logicalOp@LogicalOperatorTree(lhs, rhs)       =>
        val args = (tcExpr(lhs), tcExpr(rhs))
        args match {
          case _ if args.anyIs(Object)                                => tcBinaryOperator(logicalOp, args)
          case _ if args.bothAre(TBool)                               => TBool
          case _ if args.anyIs(TBool, TFloat, TDouble, String, Array) => ErrorOperatorDoesNotExist(logicalOp, args, expression)
          case _ if args.anyIs(TLong)                                 => TLong
          case _ if args.anyIs(TInt)                                  => TInt
          case _ if args.bothAre(TChar)                               => TInt
          case _                                                      => ErrorOperatorDoesNotExist(logicalOp, args, expression)
        }
      case shiftOp@ShiftOperatorTree(lhs, rhs)           =>
        val args = (tcExpr(lhs), tcExpr(rhs))
        args match {
          case _ if args.anyIs(Object)                                => tcBinaryOperator(shiftOp, args)
          case _ if args.anyIs(TBool, TFloat, TDouble, String, Array) => ErrorOperatorDoesNotExist(shiftOp, args, expression)
          case _ if args.anyIs(TLong)                                 => TLong
          case _                                                      => TInt
        }
      case compOp@ComparisonOperatorTree(lhs, rhs)       =>
        val args = (tcExpr(lhs), tcExpr(rhs))
        args match {
          case _ if args.anyIs(Object)               => tcBinaryOperator(compOp, args, Some(TBool))
          case _ if args.anyIs(TBool, String, Array) => ErrorOperatorDoesNotExist(compOp, args, expression)
          case _                                     => TBool
        }
      case eqOp@EqualsOperatorTree(lhs, rhs)             =>
        val args@(lhsType, rhsType) = (tcExpr(lhs), tcExpr(rhs))
        args match {
          case _ if args.anyIs(Object)                        =>
            val argList = List(lhsType, rhsType)
            val operatorType = tcBinaryOperatorNoErrors(eqOp, args)
            operatorType match {
              case TError if args.bothAre(Object) => // If both are objects they can be compared by reference
              case TError                         => ErrorOverloadedOperatorNotFound(eqOp, argList, expression)
              case _                              => correctOperatorType(eqOp, argList, Some(TBool), operatorType)
            }
          case (x, y) if x.isSubTypeOf(y) || y.isSubTypeOf(x) => // Valid
          case _ if args.anyIs(TBool, String, Array)          => ErrorOperatorDoesNotExist(eqOp, args, expression)
          case _                                              => // Valid
        }
        TBool
      case And(lhs, rhs)                                 =>
        tcExpr(lhs, TBool)
        tcExpr(rhs, TBool)
        TBool
      case Or(lhs, rhs)                                  =>
        tcExpr(lhs, TBool)
        tcExpr(rhs, TBool)
        TBool
      case notOp@Not(expr)                               =>
        tcExpr(expr, TBool, Types.Object) match {
          case x: TObject => tcUnaryOperator(notOp, x, Some(TBool))
          case _          =>
        }
        TBool
      case Is(expr, id)                                  =>
        val tpe = tcExpr(expr)
        tpe match {
          case t: TObject =>
            tcExpr(id, tpe.getSuperTypes)
            TBool
          case _          => ErrorWrongType("object", tpe, expr)
        }
      case As(expr, tpe)                                 =>
        tcExpr(expr, tpe.getType.getSuperTypes)
        tpe.getType
      case arrRead@ArrayRead(arr, index)                 =>
        val arrTpe = tcExpr(arr)
        arrTpe match {
          case TObject(classSymbol) =>
            val indexType = tcExpr(index)
            val argList = List(indexType)
            tcArrayOperator(classSymbol, arrRead, argList, arrTpe, expression)
          case TArray(arrTpe)       =>
            tcExpr(index, TInt)
            arrTpe
          case tpe                  => ErrorWrongType("array", tpe, arr)
        }
      case ArraySlice(arr, start, end)                   =>
        start.ifDefined(tcExpr(_, TInt))
        end.ifDefined(tcExpr(_, TInt))
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
        tcExpr(expr, Types.Object :: Types.Primitives) match {
          case x: TObject => tcUnaryOperator(negOp, x)
          case TChar      => TInt // Negation of char is int
          case x          => x
        }
      case hashOp@Hash(expr)                             =>
        val exprType = tcExpr(expr)
        exprType match {
          case _: TObject =>
            val argList = List(exprType)
            val operatorType = typeCheckOperator(exprType, hashOp, argList) match {
              case Some(tpe) => tpe match {
                case TInt => tpe
                case _    => ErrorWrongType(TInt, tpe, expression)
              }
              case _         => TInt
            }
            operatorType match {
              case TInt => TInt
              case _    =>
                ErrorOperatorWrongReturnType(hashOp.operatorString(argList), TInt.toString, operatorType.toString, hashOp)
            }
          case _          => TInt
        }
      case incOp@IncrementDecrementTree(obj)             =>
        obj match {
          case id: VariableID           => checkReassignment(id, expression)
          case _: ArrayRead | _: Access =>
          case _                        => ErrorInvalidIncrementDecrementExpr(expression, obj)
        }
        // TODO: Allow increment decrement for Bool types?
        tcExpr(obj, Types.Object :: Types.Primitives) match {
          case x: TObject => tcUnaryOperator(incOp, x, Some(x)) // Requires same return type as type
          case x          => x
        }
      case notOp@LogicNot(expr)                          =>
        tcExpr(expr, Types.Object, TInt, TLong, TChar) match {
          case x: TObject => tcUnaryOperator(notOp, x)
          case TLong      => TLong
          case _          => TInt
        }
      case Ternary(condition, thn, els)                  =>
        tcExpr(condition, TBool)
        val thenType = tcExpr(thn)
        tcExpr(els, thenType)
    }

    // Check result and return a valid type in case of error
    val res =
      if (expected.nonEmpty &&
        (!expected.exists(e => foundType.isSubTypeOf(e) || e.isImplicitlyConvertableFrom(foundType)))) {
        ErrorWrongType(expected, foundType, expression)
      } else {
        foundType
      }
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
      case MethodCall(meth, args)          =>
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
            TInt
          case _                    => ErrorMethodOnWrongType(methSignature, objType.toString, app)
        }
      case fieldId @ VariableID(fieldName) =>
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

    app.setType(tpe)
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

    def tcAssignmentExpr(objTpe: Type) = objTpe match {
      case obj: TObject => tcExpr(expr, obj)
      case arr: TArray  => tcExpr(expr, arr)
      case TBool        => tcExpr(expr, TBool)
      case TChar        =>
        tcExpr(expr, TInt, TChar)
        TChar
      case TInt         =>
        tcExpr(expr, TInt, TChar)
        TInt
      case TLong        =>
        tcExpr(expr, TLong, TInt, TChar)
        TLong
      case TFloat       =>
        tcExpr(expr, TFloat, TLong, TInt, TChar)
        TFloat
      case TDouble      =>
        tcExpr(expr, TDouble, TFloat, TLong, TInt, TChar)
        TDouble
      case _            => ???
    }
    to match {
      case id: VariableID           =>
        val toTpe = tcExpr(to)
        checkReassignment(id, assignment)
        tcAssignmentExpr(toTpe)
      case Access(obj, application) =>
        val toTpe = tcExpr(to)
        application match {
          case _: MethodCall  => ErrorAssignValueToMethodCall(assignment)
          case id: VariableID =>
            checkReassignment(id, assignment)
            tcAssignmentExpr(toTpe)
          case _              => ???
        }
      case ArrayRead(arr, index)    =>
        val arrTpe = tcExpr(arr)
        arrTpe match {
          case TObject(classSymbol) =>
            val (indexType, exprType) = (tcExpr(index), tcExpr(expr))

            val argList = List(indexType, exprType)
            val arrayAssignOp = ArrayAssign(Empty(), Empty(), Empty())

            val operatorType = tcArrayOperator(classSymbol, arrayAssignOp, argList, arrTpe, assignment)
            if (operatorType != TError && operatorType != TUnit) {
              val opString = arrayAssignOp.operatorString(argList, arrTpe.toString)
              ErrorOperatorWrongReturnType(opString, "Unit", operatorType.toString, expr)
            }
            exprType
          case TArray(arrayTpe)     =>
            tcExpr(index, TInt)
            tcAssignmentExpr(arrayTpe)
          case tpe                  => ErrorWrongType(arr.getType + "[]", tpe, arr)
        }
    }
  }

  def tcBinaryOperatorNoErrors(op: OperatorTree, args: (Type, Type)): Type = {
    if (args._1 == TError || args._2 == TError)
      return TError

    val argList = List(args._1, args._2)
    typeCheckOperator(args._1, op, argList) match {
      case Some(tpe)                  => tpe
      case None if args._1 != args._2 =>
        typeCheckOperator(args._2, op, argList) match {
          case Some(tpe) => tpe
          case None      => TError
        }
      case _                          => TError
    }
  }

  def tcBinaryOperator(expr: OperatorTree, args: (Type, Type), expectedType: Option[Type] = None): Type = {
    val operatorType = tcBinaryOperatorNoErrors(expr, args)
    val argList = List(args._1, args._2)
    if (operatorType == TError)
      ErrorOverloadedOperatorNotFound(expr, argList, expr)
    else
      correctOperatorType(expr, argList, expectedType, operatorType)
  }

  def tcUnaryOperator(expr: OperatorTree, arg: Type, expectedType: Option[Type] = None): Type = {
    val argList = List(arg)
    val operatorType = typeCheckOperator(arg, expr, argList) match {
      case Some(tpe) => tpe
      case None      => ErrorOverloadedOperatorNotFound(expr, argList, expr)
    }
    correctOperatorType(expr, argList, expectedType, operatorType)
  }

  def tcArrayOperator(classSymbol: ClassSymbol, opType: ArrayOperatorTree, argList: List[Type], arrTpe: Type, pos: Positioned) = {
    classSymbol.lookupOperator(opType, argList) match {
      case Some(operatorSymbol) =>
        checkOperatorPrivacy(classSymbol, operatorSymbol, pos)
        inferTypeOfMethod(operatorSymbol)
        operatorSymbol.getType
      case None                 =>
        ErrorIndexingOperatorNotFound(opType, argList, arrTpe.toString, pos)
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

  def checkCorrectOverrideReturnTypes(prog: Program) =
    prog.classes.foreach { clazz =>
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


  def checkTraitsAreImplemented(prog: Program) =
    prog.classes.filter(!_.isAbstract).foreach { classDecl =>
      classDecl.implementedTraits.foreach(t => traitIsImplemented(classDecl, t.getSymbol))
    }

  private def checkReassignment(id: VariableID, pos: Positioned) =
    if (id.hasSymbol && id.getSymbol.isFinal)
      ErrorReassignmentToVal(id.name, pos)


  private def checkNullAssignment(to: Type, from: Type, pos: Positioned) =
    if (from == TNull && !to.isInstanceOf[TNullable])
      ErrorAssignNullToNonNullable(to, pos)


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
                if (hasNextMethod.getType != TBool)
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
      new TypeChecker(ctx, methodSymbol, currentMethodSymbol :: methodStack).tcMethod()
  }

  private def typeCheckOperator(classType: Type, operator: OperatorTree, args: List[Type]): Option[Type] =
    classType match {
      case TObject(classSymbol) =>
        classSymbol.lookupOperator(operator, args) match {
          case Some(operatorSymbol) =>
            checkOperatorPrivacy(classSymbol, operatorSymbol, operator)
            inferTypeOfMethod(operatorSymbol)
            operator.setType(operatorSymbol.getType)
            Some(operatorSymbol.getType)
          case None                 => None
        }
      case _                    => None
    }

  private def correctOperatorType(op: OperatorTree, args: List[Type], expectedType: Option[Type], found: Type): Type = {

    // No need to report another error if one has already been found
    if (found == TError)
      return TError

    expectedType match {
      case Some(expected) =>
        if (found != expected)
          ErrorOperatorWrongReturnType(op.operatorString(args), expected.toString, found.toString, op)
        else found
      case _              => found
    }
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
    if (!checkPrivacy(classSymbol, methodSymbol.accessability))
      ErrorConstructorPrivacy(methodSymbol, classSymbol.name, currentMethodSymbol.classSymbol.name, pos)

  private def checkMethodPrivacy(classSymbol: ClassSymbol, methodSymbol: MethodSymbol, pos: Positioned): Unit =
    if (!checkPrivacy(classSymbol, methodSymbol.accessability))
      ErrorMethodPrivacy(methodSymbol, classSymbol.name, currentMethodSymbol.classSymbol.name, pos)

  private def checkFieldPrivacy(classSymbol: ClassSymbol, varSymbol: FieldSymbol, pos: Positioned): Unit =
    if (!checkPrivacy(classSymbol, varSymbol.accessability))
      ErrorFieldPrivacy(varSymbol, classSymbol.name, currentMethodSymbol.classSymbol.name, pos)

  private def checkOperatorPrivacy(classSymbol: ClassSymbol, opSymbol: OperatorSymbol, pos: Positioned): Unit =
    if (!checkPrivacy(classSymbol, opSymbol.accessability))
      ErrorOperatorPrivacy(opSymbol, classSymbol.name, currentMethodSymbol.classSymbol.name, pos)

  private def checkPrivacy(classSymbol: ClassSymbol, access: Accessability) = access match {
    case Public()                                                                                => true
    case Private() if classSymbol == currentMethodSymbol.classSymbol                             => true
    case Protected() if currentMethodSymbol.classSymbol.getType.isSubTypeOf(classSymbol.getType) => true
    case _                                                                                       => false
  }

}
