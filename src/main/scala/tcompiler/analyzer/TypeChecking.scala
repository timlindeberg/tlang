package tcompiler
package analyzer

import tcompiler.analyzer.Symbols._
import tcompiler.analyzer.Types._
import tcompiler.ast.TreeGroups._
import tcompiler.ast.Trees
import tcompiler.ast.Trees._
import tcompiler.utils._

import scala.collection.mutable.ArrayBuffer

object TypeChecking extends Pipeline[Program, Program] {

  val hasBeenTypechecked = scala.collection.mutable.Set[MethodSymbol]()

  /**
   * Typechecking does not produce a value, but has the side effect of
   * attaching types to trees and potentially outputting error messages.
   */
  def run(ctx: Context)(prog: Program): Program = {
    val methodDecl = MethodDecl(None, Identifier(""), List(), Block(List()), Set(Private))

    // Typecheck fields
    prog.classes.foreach { classDecl =>
      val typeChecker = new TypeChecker(ctx, new MethodSymbol("", classDecl.getSymbol, methodDecl))
      classDecl.vars.foreach(typeChecker.tcStat(_))
    }

    // Typecheck methods
    prog.classes.foreach { classDecl =>
      classDecl.methods.foreach(method => new TypeChecker(ctx, method.getSymbol).tcMethod())
    }
    prog
  }
}

class TypeChecker(ctx: Context, currentMethodSymbol: MethodSymbol, methodStack: List[MethodSymbol] = List()) {

  val returnStatements = ArrayBuffer[(Return, Type)]()

  def tcMethod(): Unit = {
    if (TypeChecking.hasBeenTypechecked(currentMethodSymbol))
      return

    if (currentMethodSymbol.getType == TUntyped && methodStack.contains(currentMethodSymbol)) {
      ErrorCantInferTypeRecursiveMethod(currentMethodSymbol)
      return
    }

    tcStat(currentMethodSymbol.ast.stat)

    if (currentMethodSymbol.getType != TUntyped)
      return

    if (returnStatements.isEmpty) {
      currentMethodSymbol.setType(TUnit)
      return
    }

    if (returnStatements.map(_._2).toSet.size > 1) {
      val s = returnStatements.map { case (stat, tpe) => "Line " + stat.line + ": " + tpe }.mkString(", ")
      ErrorMultipleReturnTypes(s, returnStatements.head._1)
      return
    }

    currentMethodSymbol.setType(returnStatements.head._2)
    TypeChecking.hasBeenTypechecked += currentMethodSymbol
  }

  def tcStat(statement: StatTree): Unit = statement match {
    case Block(stats)                                =>
      stats.foreach(tcStat)
    case varDecl @ VarDecl(tpe, id, init, modifiers) =>
      tpe match {
        case Some(t) => init match {
          case Some(expr) => tcExpr(expr, t.getType)
          case _          =>
        }
        case None    => init match {
          case Some(expr) =>
            val inferedType = tcExpr(expr)
            id.setType(inferedType)
          case _          => ErrorNoTypeNoInitalizer(varDecl)
        }
      }
    case If(expr, thn, els)                          =>
      tcExpr(expr, TBool)
      tcStat(thn)
      if (els.isDefined)
        tcStat(els.get)
    case While(expr, stat)                           =>
      tcExpr(expr, TBool)
      tcStat(stat)

    case For(init, condition, post, stat) =>
      init.foreach(tcStat(_))
      tcExpr(condition, TBool)
      post.foreach(tcStat)
      tcStat(stat)
    case PrintStatement(expr)             =>
      if (tcExpr(expr) == TUnit) ErrorPrintUnit(expr)
    case Error(expr)                      =>
      tcExpr(expr, TString)
    case ret @ Return(Some(expr))         =>
      if (currentMethodSymbol.ast.retType.isDefined)
        returnStatements += ((ret, tcExpr(expr, currentMethodSymbol.getType)))
      else
        returnStatements += ((ret, tcExpr(expr)))
    case ret @ Return(None)               =>
      if (currentMethodSymbol.ast.retType.isDefined && currentMethodSymbol.getType != TUnit)
        ErrorWrongReturnType(currentMethodSymbol.getType.toString, ret)
      returnStatements += ((ret, TUnit))
    case expr: ExprTree                   =>
      tcExpr(expr)
  }

  def tcExpr(expr: ExprTree, expected: Type*): Type = {
    tcExpr(expr, expected.toList)
  }

  def tcExpr(expression: ExprTree, expected: List[Type]): Type = {
    val foundType = expression match {
      case _: IntLit    => TInt
      case _: LongLit   => TLong
      case _: FloatLit  => TFloat
      case _: DoubleLit => TDouble
      case _: CharLit   => TChar
      case _: StringLit => TString
      case _: True      => TBool
      case _: False     => TBool

      case id: Identifier                  =>
        id.getSymbol match {
          case varSymbol: VariableSymbol => varSymbol.classSymbol match {
            case Some(clazz) => checkFieldPrivacy(clazz, varSymbol)
            case None        =>
          }
          case _                         =>
        }
        id.getType
      case id: ClassIdentifier             => id.getType
      case th: This                        => th.getSymbol.getType
      case mc: MethodCall                  => tcMethodCall(mc)
      case fr @ FieldRead(obj, id)         =>
        val tpe = typeCheckField(obj, id, fr)
        fr.setType(tpe)
        tpe
      case fa @ FieldAssign(obj, id, expr) =>
        val tpe = typeCheckField(obj, id, fa)
        fa.setType(tpe)
        tcExpr(expr, tpe)
        tpe
      case newArray @ NewArray(tpe, sizes) =>
        sizes.foreach(tcExpr(_, TInt))
        var arrayType = tpe.getType
        for (i <- 1 to newArray.dimension)
          arrayType = TArray(arrayType)
        arrayType
      case ArrayLit(expressions)           =>
        val tpes = expressions.map(tcExpr(_))
        if (tpes.isEmpty) {
          TArray(Types.anyObject)
        } else {
          val typeSet = tpes.toSet
          if (typeSet.size > 1)
            ErrorMultipleArrayLitTypes(typeSet.mkString(", "), expression)
          TArray(tpes.head)
        }
      case Plus(lhs, rhs)                  =>
        val types = List(Types.anyObject, TString, TBool) ::: Types.primitives
        val argTypes = (tcExpr(lhs, types), tcExpr(rhs, types))
        argTypes match {
          case (TString, _) | (_, TString)       => TString
          case (_, _: TObject) | (_: TObject, _) => tcBinaryOperator(expression, argTypes)
          case (TBool, x)                        => ErrorWrongType(x, "Bool", lhs)
          case (x, TBool)                        => ErrorWrongType(x, "Bool", rhs)
          case (TDouble, _) | (_, TDouble)       => TDouble
          case (TFloat, _) | (_, TFloat)         => TFloat
          case (TLong, _) | (_, TLong)           => TLong
          case _                                 => TInt
        }
      case BinaryOperator(lhs, rhs)        =>
        val types = List(Types.anyObject, TString) ::: Types.primitives
        val argTypes = (tcExpr(lhs, types), tcExpr(rhs, types))
        argTypes match {
          case (_, _: TObject) | (_: TObject, _) => tcBinaryOperator(expression, argTypes)
          case (TString, _) | (_, TString)       => ErrorOperatorNotFound(expression, argTypes, expression)
          case (TDouble, _) | (_, TDouble)       => TDouble
          case (TFloat, _) | (_, TFloat)         => TFloat
          case (TLong, _) | (_, TLong)           => TLong
          case _                                 => TInt
        }
      case LogicalOperator(lhs, rhs)       =>
        val types = List(Types.anyObject, TString, TBool, TInt, TLong, TChar)
        val argTypes = (tcExpr(lhs, types), tcExpr(rhs, types))
        argTypes match {
          case (_: TObject, _) | (_, _: TObject) => tcBinaryOperator(expression, argTypes)
          case (TString, _) | (_, TString)       => ErrorOperatorNotFound(expression, argTypes, expression)
          case (TBool, TBool)                    => TBool
          case (TBool, x)                        => ErrorWrongType(x, "Bool", lhs)
          case (x, TBool)                        => ErrorWrongType(x, "Bool", rhs)
          case (TLong, _) | (_, TLong)           => TLong
          case (TInt, _) | (_, TInt)             => TInt
          case (TChar, TChar)                    => TInt
          case (TChar, x)                        => ErrorWrongType(x, "Char", lhs)
          case (x, TChar)                        => ErrorWrongType(x, "Char", rhs)
          case _                                 => TError
        }
      case ShiftOperator(lhs, rhs)         =>
        val types = List(Types.anyObject, TString, TInt, TLong, TChar)
        val argTypes = (tcExpr(lhs, types), tcExpr(rhs, types))
        argTypes match {
          case (_: TObject, _) | (_, _: TObject) => tcBinaryOperator(expression, argTypes)
          case (TString, _) | (_, TString)       => ErrorOperatorNotFound(expression, argTypes, expression)
          case (TLong, _) | (_, TLong)           => TLong
          case _                                 => TInt
        }
      case Assign(id, expr)                =>
        id.getType match {
          case objType: TObject => tcExpr(expr, objType)
          case arrType: TArray  => tcExpr(expr, arrType)
          case TString          => tcExpr(expr, TString)
          case TBool            => tcExpr(expr, TBool)
          case TChar            =>
            tcExpr(expr, TInt, TChar)
            TChar
          case TInt             =>
            tcExpr(expr, TInt, TChar)
            TInt
          case TLong            =>
            tcExpr(expr, TLong, TInt, TChar)
            TLong
          case TFloat           =>
            tcExpr(expr, TFloat, TLong, TInt, TChar)
            TFloat
          case TDouble          =>
            tcExpr(expr, TDouble, TFloat, TLong, TInt, TChar)
            TDouble
          case _                => TError
        }
      case ArrayAssign(arr, index, expr)   =>
        val arrTpe = tcExpr(arr)
        arrTpe match {
          case TObject(classSymbol) =>
            val (indexType, exprType) = (tcExpr(index), tcExpr(expr))

            val argList = List(indexType, exprType)

            val operatorType = classSymbol.lookupOperator(expression, argList) match {
              case Some(operatorSymbol) =>
                checkOperatorPrivacy(classSymbol, operatorSymbol)
                if (operatorSymbol.getType == TUntyped) {
                  new TypeChecker(ctx, operatorSymbol, currentMethodSymbol :: methodStack).tcMethod()
                }
                expression.setType(operatorSymbol.getType)
                operatorSymbol.getType
              case None                 =>
                ErrorOperatorNotFound(expression, argList, expression)
            }

            if (operatorType != TError && operatorType != TUnit)
              ErrorOperatorWrongReturnType(Trees.operatorString(expression, argList), "Unit", operatorType.toString, expr)
            exprType
          case TArray(arrayTpe)     =>
            tcExpr(index, TInt)
            arrayTpe match {
              case objType: TObject => tcExpr(expr, objType)
              case arrType: TArray  => tcExpr(expr, arrType)
              case TString          => tcExpr(expr, TString)
              case TBool            => tcExpr(expr, TBool)
              case TChar            =>
                tcExpr(expr, TInt, TChar)
                TChar
              case TInt             =>
                tcExpr(expr, TInt, TChar)
                TInt
              case TLong            =>
                tcExpr(expr, TLong, TInt, TChar)
                TLong
              case TFloat           =>
                tcExpr(expr, TFloat, TLong, TInt, TChar)
                TFloat
              case TDouble          =>
                tcExpr(expr, TDouble, TFloat, TLong, TInt, TChar)
                TDouble
            }
          case tpe                  => ErrorWrongType(arr.getType + "[]", tpe, arr)
        }
      case ComparisonOperator(lhs, rhs)    =>
        val types = List(Types.anyObject, TString) ::: Types.primitives
        val argTypes = (tcExpr(lhs, types), tcExpr(rhs, types))
        argTypes match {
          case (_: TObject, _) | (_, _: TObject) => tcBinaryOperator(expression, argTypes, Some(TBool))
          case (TString, _) | (_, TString)       => ErrorOperatorNotFound(expression, argTypes, expression)
          case _                                 =>
        }
        TBool
      case EqualsOperator(lhs, rhs)        =>
        val argTypes @ (lhsType, rhsType) = (tcExpr(lhs), tcExpr(rhs))
        argTypes match {
          case (_: TObject, _) | (_, _: TObject) =>
            val argList = List(lhsType, rhsType)
            val operatorType = typeCheckOperator(lhsType, expression, argList) match {
              case Some(tpe)                  => tpe
              case None if lhsType != rhsType =>
                typeCheckOperator(rhsType, expression, argList) match {
                  case Some(tpe) => tpe
                  case None      => TError
                }
              case _                          => TError
            }

            operatorType match {
              case TError =>
                // If both are objects they can be compared by reference
                if (!lhsType.isInstanceOf[TObject] || !rhsType.isInstanceOf[TObject])
                  ErrorOperatorNotFound(expression, argList, expression)
              case _      => correctOperatorType(expression, argList, Some(TBool), operatorType)
            }
          case _                                 =>
            lhsType match {
              case arrayType: TArray =>
                if (!rhsType.isSubTypeOf(arrayType))
                  ErrorWrongType(arrayType, rhsType, expression)
              case TString           =>
                if (!rhsType.isSubTypeOf(TString))
                  ErrorWrongType("String", rhsType, expression)
              case TBool             =>
                if (!rhsType.isSubTypeOf(TBool))
                  ErrorWrongType("Bool", rhsType, expression)
              case _                 =>
            }
        }
        TBool
      case And(lhs, rhs)                   =>
        tcExpr(lhs, TBool)
        tcExpr(rhs, TBool)
        TBool
      case Or(lhs, rhs)                    =>
        tcExpr(lhs, TBool)
        tcExpr(rhs, TBool)
        TBool
      case Not(expr)                       =>
        tcExpr(expr, TBool, Types.anyObject) match {
          case x: TObject => tcUnaryOperator(expression, x, Some(TBool))
          case _          =>
        }
        TBool
      case Instance(expr, id)              =>
        val tpe = tcExpr(expr)
        tpe match {
          case t: TObject =>
            tcExpr(id, tpe.getSuperTypes)
            TBool
          case _          => ErrorWrongType("object", tpe, expr)
        }
      case As(expr, tpe)                   =>
        tcExpr(expr, tpe.getType.getSuperTypes)
        tpe.getType
      case ArrayRead(arr, index)           =>
        val arrTpe = tcExpr(arr)
        arrTpe match {
          case TObject(classSymbol) =>
            val indexType = tcExpr(index)
            val argList = List(indexType)

            classSymbol.lookupOperator(expression, argList) match {
              case Some(operatorSymbol) =>
                checkOperatorPrivacy(classSymbol, operatorSymbol)
                if (operatorSymbol.getType == TUntyped) {
                  new TypeChecker(ctx, operatorSymbol, currentMethodSymbol :: methodStack).tcMethod()
                }
                expression.setType(operatorSymbol.getType)
                operatorSymbol.getType
              case None                 =>
                ErrorOperatorNotFound(expression, argList, expression)
            }
          case TArray(arrTpe)       =>
            tcExpr(index, TInt)
            arrTpe
          case tpe                  => ErrorWrongType("array", tpe, arr)
        }
      case ArrayLength(arr)                =>
        tcExpr(arr) match {
          case TArray(_) => TInt
          case tpe       => ErrorWrongType("array", tpe, arr)
        }
      case newDecl @ New(tpe, exprs)       =>
        val argTypes = exprs.map(tcExpr(_))

        tpe.getType match {
          case TObject(classSymbol) =>
            classSymbol.lookupMethod("new", argTypes) match {
              case Some(constructorSymbol) => checkConstructorPrivacy(classSymbol, constructorSymbol)
              case None if exprs.nonEmpty  =>
                val methodSignature = tpe.value + exprs.map(_.getType).mkString("(", " , ", ")")
                ErrorDoesntHaveConstructor(classSymbol.name, methodSignature, newDecl)
              case _                       =>
            }
          case primitiveType        => ErrorNewPrimitive(primitiveType.toString, newDecl)
        }
        tpe.getType
      case Negation(expr)                  =>
        tcExpr(expr, Types.anyObject :: Types.primitives) match {
          case x: TObject => tcUnaryOperator(expression, x)
          case TChar      => TInt // Negation of char is int
          case x          => x
        }
      case Hash(expr)                      =>
        val exprType = tcExpr(expr, TString :: Types.anyObject :: Types.primitives)
        exprType match {
          case _: TObject =>
            val argList = List(exprType)
            val operatorType = typeCheckOperator(exprType, expression, argList) match {
              case Some(tpe) => tpe match {
                case TInt => tpe
                case _    => ErrorWrongType(TInt, tpe, expression)
              }
              case _         => TInt
            }
            operatorType match {
              case TInt => TInt
              case _    =>
                ErrorOperatorWrongReturnType(Trees.operatorString(expression, argList), "Int", operatorType.toString, expression)
            }
          case _          => TInt
        }
      case IncrementDecrement(id)          =>
        id match {
          case _: Identifier | _: ArrayRead | _: FieldRead =>
          case _                                           => ErrorInvalidIncrementDecrementExpr(expression, id)
        }
        tcExpr(id, Types.anyObject :: Types.primitives) match {
          case x: TObject => tcUnaryOperator(expression, x, Some(x)) // Requires same return type as type
          case x          => x
        }
      case LogicNot(expr)                  =>
        tcExpr(expr, Types.anyObject, TInt, TLong, TChar) match {
          case x: TObject => tcUnaryOperator(expression, x)
          case TLong      => TLong
          case _          => TInt
        }
      case Ternary(condition, thn, els)    =>
        tcExpr(condition, TBool)
        val thenType = tcExpr(thn)
        tcExpr(els, thenType)
    }

    // Check result and return a valid type in case of error
    val res =
      if (expected.nonEmpty &&
        (!expected.exists(e => foundType.isSubTypeOf(e) || e.isImplicitlyConvertableFrom(foundType)))) {
        ErrorWrongType(makeExpectedString(expected), foundType, expression)
      } else {
        foundType
      }
    expression.setType(res)
    res
  }

  def tcMethodCall(mc: MethodCall) = {
    val obj = mc.obj
    val meth = mc.meth
    val methodCallArgs = mc.args

    val objType = tcExpr(obj)
    val argTypes = methodCallArgs.map(tcExpr(_))

    val tpe = objType match {
      case TObject(classSymbol) =>
        classSymbol.lookupMethod(meth.value, argTypes) match {
          case Some(methSymbol) =>
            checkMethodPrivacy(classSymbol, methSymbol)
            checkStaticMethodConstraints(obj, classSymbol, methSymbol, meth)
            if (methSymbol.getType == TUntyped) {
              new TypeChecker(ctx, methSymbol, currentMethodSymbol :: methodStack).tcMethod()
            }
            meth.setSymbol(methSymbol)
            meth.getType
          case None             =>
            val methodSignature = meth.value + methodCallArgs.map(_.getType).mkString("(", ", ", ")")
            ErrorClassDoesntHaveMethod(classSymbol.name, methodSignature, mc)
        }
      case _                    => ErrorMethodOnWrongType(objType.toString, mc)
    }
    mc.setType(tpe)
    tpe
  }

  def tcBinaryOperator(expr: ExprTree, args: (Type, Type), expectedType: Option[Type] = None): Type = {
    val argList = List(args._1, args._2)
    val operatorType = typeCheckOperator(args._1, expr, argList) match {
      case Some(tpe)                  =>
        tpe
      case None if args._1 != args._2 =>
        typeCheckOperator(args._2, expr, argList) match {
          case Some(tpe) => tpe
          case None      => ErrorOperatorNotFound(expr, argList, expr)
        }
      case _                          => ErrorOperatorNotFound(expr, argList, expr)
    }
    correctOperatorType(expr, argList, expectedType, operatorType)
  }

  def tcUnaryOperator(expr: ExprTree, arg: Type, expectedType: Option[Type] = None): Type = {
    val argList = List(arg)
    val operatorType = typeCheckOperator(arg, expr, argList) match {
      case Some(tpe) => tpe
      case None      => ErrorOperatorNotFound(expr, argList, expr)
    }
    correctOperatorType(expr, argList, expectedType, operatorType)
  }


  private def typeCheckOperator(classType: Type, expr: ExprTree, args: List[Type]): Option[Type] =
    classType match {
      case TObject(classSymbol) =>
        classSymbol.lookupOperator(expr, args) match {
          case Some(operatorSymbol) =>
            checkOperatorPrivacy(classSymbol, operatorSymbol)
            if (operatorSymbol.getType == TUntyped) {
              new TypeChecker(ctx, operatorSymbol, currentMethodSymbol :: methodStack).tcMethod()
            }
            expr.setType(operatorSymbol.getType)
            Some(operatorSymbol.getType)
          case None                 => None
        }
      case _                    => None
    }

  private def correctOperatorType(expr: ExprTree, args: List[Type], expectedType: Option[Type], found: Type): Type =
    expectedType match {
      case Some(expected) =>
        if (found != expected)
          ErrorOperatorWrongReturnType(Trees.operatorString(expr, args), expected.toString, found.toString, expr)
        else found
      case _              => found
    }


  private def typeCheckField(obj: ExprTree, fieldId: Identifier, pos: Positioned) = {
    val objType = tcExpr(obj)

    objType match {
      case TObject(classSymbol) =>
        classSymbol.lookupVar(fieldId.value) match {
          case Some(varSymbol) =>
            checkFieldPrivacy(classSymbol, varSymbol)
            checkStaticFieldConstraints(obj, classSymbol, varSymbol, fieldId)
            fieldId.setSymbol(varSymbol)
            fieldId.getType
          case None            =>
            ErrorClassDoesntHaveField(classSymbol.name, fieldId.value, pos)
        }
      case _                    => ErrorFieldOnWrongType(objType.toString, pos)
    }
  }

  private def checkStaticMethodConstraints(obj: ExprTree, classSymbol: ClassSymbol, methodSymbol: MethodSymbol, pos: Positioned) = {
    if (!methodSymbol.isStatic && isStaticCall(obj))
      ErrorNonStaticMethodAsStatic(methodSymbol.name, pos)

    if (obj.isInstanceOf[This] && currentMethodSymbol.isStatic && !methodSymbol.isStatic)
      ErrorNonStaticMethodFromStatic(methodSymbol.name, pos)
  }

  private def checkStaticFieldConstraints(obj: ExprTree, classSymbol: ClassSymbol, varSymbol: VariableSymbol, pos: Positioned) = {
    if (!varSymbol.isStatic && isStaticCall(obj))
      ErrorNonStaticFieldAsStatic(varSymbol.name, pos)

    if (obj.isInstanceOf[This] && currentMethodSymbol.isStatic && !varSymbol.isStatic)
      ErrorNonStaticFieldFromStatic(varSymbol.name, pos)
  }

  private def checkConstructorPrivacy(classSymbol: ClassSymbol, methodSymbol: MethodSymbol): Unit =
    if (!checkPrivacy(classSymbol, methodSymbol.accessability))
      ErrorConstructorPrivacy(accessabilityString(methodSymbol), classSymbol.name, currentMethodSymbol.classSymbol.name, methodSymbol)

  private def checkMethodPrivacy(classSymbol: ClassSymbol, methodSymbol: MethodSymbol): Unit =
    if (!checkPrivacy(classSymbol, methodSymbol.accessability))
      ErrorMethodPrivacy(accessabilityString(methodSymbol), methodSymbol.name, classSymbol.name, currentMethodSymbol.classSymbol.name, methodSymbol)

  private def checkFieldPrivacy(classSymbol: ClassSymbol, varSymbol: VariableSymbol): Unit =
    if (!checkPrivacy(classSymbol, varSymbol.accessability))
      ErrorFieldPrivacy(accessabilityString(varSymbol), varSymbol.name, classSymbol.name, currentMethodSymbol.classSymbol.name, varSymbol)

  private def checkOperatorPrivacy(classSymbol: ClassSymbol, opSymbol: OperatorSymbol): Unit =
    if (!checkPrivacy(classSymbol, opSymbol.accessability))
      ErrorOperatorPrivacy(accessabilityString(opSymbol), Trees.operatorString(opSymbol), classSymbol.name, currentMethodSymbol.classSymbol.name, opSymbol)

  private def checkPrivacy(classSymbol: ClassSymbol, access: Accessability) = access match {
    case Public                                                                                => true
    case Private if classSymbol == currentMethodSymbol.classSymbol                             => true
    case Protected if currentMethodSymbol.classSymbol.getType.isSubTypeOf(classSymbol.getType) => true
    case _                                                                                     => false
  }

  private def accessabilityString(modifiable: Modifiable) = modifiable.accessability.toString.toLowerCase

  private def makeExpectedString(expected: List[Type]): String = expected.size match {
    case 0 => ""
    case 1 => expected.head.toString
    case n => expected.take(n - 1).mkString(", ") + " or " + expected.last
  }

  private def error(errorCode: Int, msg: String, pos: Positioned) = {
    ctx.reporter.error("T", errorCode, msg, pos)
    TError
  }

  //---------------------------------------------------------------------------------------
  //  Error messages
  //---------------------------------------------------------------------------------------
  private def ErrorWrongType(expected: Type, found: Type, pos: Positioned): Type = ErrorWrongType(expected.toString, found.toString, pos)
  private def ErrorWrongType(expected: Type, found: String, pos: Positioned): Type = ErrorWrongType(expected.toString, found, pos)
  private def ErrorWrongType(expected: String, found: Type, pos: Positioned): Type = ErrorWrongType(expected, found.toString, pos)
  private def ErrorWrongType(expected: String, found: String, pos: Positioned): Type =
    error(0, s"Expected type: '$expected', found: '$found'.", pos)

  private def ErrorClassDoesntHaveMethod(className: String, methodSignature: String, pos: Positioned) =
    error(1, s"Class '$className' does not contain a method '$methodSignature'.", pos)

  private def ErrorMethodOnWrongType(tpe: String, pos: Positioned) =
    error(2, s"Cannot call method on type '$tpe'.", pos)

  private def ErrorClassDoesntHaveField(className: String, fieldName: String, pos: Positioned) =
    error(3, s"Class '$className' does not contain a field '$fieldName'.", pos)

  private def ErrorFieldOnWrongType(tpe: String, pos: Positioned) =
    error(4, s"Cannot acces field on type '$tpe'.", pos)

  private def ErrorNonStaticMethodAsStatic(methodName: String, pos: Positioned) =
    error(5, s"Trying to call method '$methodName' statically but the method is not declared as static.", pos)

  private def ErrorNonStaticMethodFromStatic(methodName: String, pos: Positioned) =
    error(6, s"Cannot access non-static method '$methodName' from a static method.", pos)

  private def ErrorNonStaticFieldAsStatic(fieldName: String, pos: Positioned) =
    error(7, s"Trying to access field '$fieldName' statically but the field is not declared as static.", pos)

  private def ErrorNonStaticFieldFromStatic(fieldName: String, pos: Positioned) =
    error(8, s"Cannot access non-static field '$fieldName' from a static method.", pos)

  private def ErrorConstructorPrivacy(accessability: String, className: String, callingClass: String, pos: Positioned) =
    error(9, s"Cannot call $accessability constructor of '$className' from class '$callingClass'.", pos)

  private def ErrorMethodPrivacy(accessability: String, methodName: String, className: String, callingClass: String, pos: Positioned) =
    error(10, s"Cannot call $accessability method '$methodName' in '$className' from class '$callingClass'.", pos)

  private def ErrorFieldPrivacy(accessability: String, fieldName: String, className: String, callingClass: String, pos: Positioned) =
    error(11, s"Cannot access $accessability field '$fieldName' in '$className' from class '$callingClass'.", pos)

  private def ErrorOperatorPrivacy(accessability: String, operatorName: String, className: String, callingClass: String, pos: Positioned) =
    error(12, s"Cannot call $accessability operator '$operatorName' in '$className' from class '$callingClass'.", pos)

  private def ErrorOperatorNotFound(expr: ExprTree, args: (Type, Type), pos: Positioned): Type = ErrorOperatorNotFound(expr, List(args._1, args._2), pos)
  private def ErrorOperatorNotFound(expr: ExprTree, args: List[Type], pos: Positioned): Type = {
    val classesString = if (args.size == 2 && args.head != args(1))
      "Classes " + args.mkString(" or ") + " do"
    else
      "Class " + args.head + " does"
    val operatorName = Trees.operatorString(expr, args)
    error(13, s"$classesString not contain an operator '$operatorName'.", pos)
  }

  private def ErrorOperatorWrongReturnType(operator: String, expected: String, found: String, pos: Positioned) =
    error(14, s"Operator '$operator' has wrong return type: expected '$expected', found '$found'.", pos)

  private def ErrorPrintUnit(pos: Positioned) =
    error(15, "Cannot print type Unit.", pos)

  private def ErrorWrongReturnType(tpe: String, pos: Positioned) =
    error(16, s"Expected a return value of type '$tpe'.", pos)

  private def ErrorDoesntHaveConstructor(className: String, methodSignature: String, pos: Positioned) =
    error(17, s"Class '$className' does not contain a constructor '$methodSignature'.", pos)

  private def ErrorNewPrimitive(tpe: String, pos: Positioned) =
    error(18, s"Cannot create a new instance of primitive type '$tpe'.", pos)

  private def ErrorNoTypeNoInitalizer(pos: Positioned) =
    error(19, "Cannot declare variable without type or initialization.", pos)

  private def ErrorMultipleReturnTypes(typeList: String, pos: Positioned) =
    error(20, s"Method contains return statements of multiple types: $typeList", pos)

  private def ErrorMultipleArrayLitTypes(typeList: String, pos: Positioned) =
    error(21, s"Array literal contains multiple types: $typeList", pos)

  private def ErrorCantInferTypeRecursiveMethod(pos: Positioned) =
    error(22, s"Can't infer type of recursive method.", pos)

  private def ErrorInvalidIncrementDecrementExpr(expr: ExprTree, pos: Positioned) = {
    val incrementOrDecrement = expr match {
      case _: PreIncrement | _: PostIncrement => "increment"
      case _: PreDecrement | _: PostDecrement => "decrement"
    }
    error(23, s"Invalid $incrementOrDecrement expression.", pos)
  }


}
