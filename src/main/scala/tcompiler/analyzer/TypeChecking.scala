package tcompiler
package analyzer

import tcompiler.analyzer.Symbols.{OperatorSymbol, ClassSymbol, MethodSymbol, VariableSymbol}
import tcompiler.analyzer.Types._
import tcompiler.ast.TreeGroups._
import tcompiler.ast.Trees
import tcompiler.ast.Trees._
import tcompiler.utils._

object TypeChecking extends Pipeline[Program, Program] {


  /**
   * Typechecking does not produce a value, but has the side effect of
   * attaching types to trees and potentially outputting error messages.
   */
  def run(ctx: Context)(prog: Program): Program = {

    if (prog.main.isDefined) {
      val main = prog.main.get
      val mainMethod = new MethodSymbol("main", main.getSymbol, Private).setType(TUnit)
      val mainTypeChecker = new TypeChecker(ctx, mainMethod)
      main.stats.foreach(mainTypeChecker.tcStat)
    }

    prog.classes.foreach { classDecl =>
      classDecl.vars.foreach { varDecl =>
        val method = new MethodSymbol("tmp", classDecl.getSymbol, Private).setType(TUnit)
        val typeChecker = new TypeChecker(ctx, method)
        varDecl match {
          case VarDecl(tpe, id, Some(expr), _) => typeChecker.tcExpr(expr, tpe.getType)
          case _                               =>
        }
      }
      classDecl.methods.foreach { method =>
        val typeChecker = new TypeChecker(ctx, method.getSymbol)
        method.vars.foreach {
          case VarDecl(tpe, id, Some(expr), _) => typeChecker.tcExpr(expr, tpe.getType)
          case _                               =>
        }
        method.stats.foreach(typeChecker.tcStat)
      }
    }
    prog
  }
}

class TypeChecker(ctx: Context, currentMethodSymbol: MethodSymbol) {
  def typeCheckMethodCall(mc: MethodCall) = {
    val obj = mc.obj
    val meth = mc.meth
    val methodCallArgs = mc.args
    def methodSignature = meth.value + methodCallArgs.map(_.getType).mkString("(", ", ", ")")

    val objType = tcExpr(obj)
    val argTypes = methodCallArgs.map(tcExpr(_))

    val tpe = objType match {
      case TObject(classSymbol) =>
        classSymbol.lookupMethod(meth.value, argTypes) match {
          case Some(methSymbol) =>
            checkMethodPrivacy(classSymbol, methSymbol)
            meth.setSymbol(methSymbol)
            meth.getType
          case None             =>
            error("Class \'" + classSymbol.name + "\' does not contain a method \'" + methodSignature + "\'.", mc)
        }
      case _                    => error("Cannot call function on type " + objType, mc)
    }
    mc.setType(tpe)
    tpe
  }

  def typeCheckFieldRead(fr: FieldRead) = {
    val obj = fr.obj
    val fieldId = fr.id

    val objType = tcExpr(obj)

    val tpe = objType match {
      case TObject(classSymbol) =>
        classSymbol.lookupVar(fieldId.value) match {
          case Some(varSymbol) =>
            checkFieldPrivacy(classSymbol, varSymbol)
            fieldId.setSymbol(varSymbol)
            fieldId.getType
          case None            =>
            error("Class \'" + classSymbol.name + "\' does not contain a field \'" + fieldId.value + "\'.", fr)
        }
      case _                    => error("Cannot acces field on type " + objType, fr)
    }
    fr.setType(tpe)
    tpe
  }

  def typeCheckFieldAssign(fa: FieldAssign) = {
    val obj = fa.obj
    val fieldId = fa.id

    val objType = tcExpr(obj)

    val tpe = objType match {
      case TObject(classSymbol) =>
        classSymbol.lookupVar(fieldId.value) match {
          case Some(varSymbol) =>
            checkFieldPrivacy(classSymbol, varSymbol)
            fieldId.setSymbol(varSymbol)
            fieldId.getType
          case None            =>
            error("Class \'" + classSymbol.name + "\' does not contain a field \'" + fieldId.value + "\'.", fa)
        }
      case _                    => error("Cannot acces field on type " + objType, fa)
    }
    fa.setType(tpe)
    tcExpr(fa.expr, tpe)
    tpe
  }

  def _checkPrivacy(classSymbol: ClassSymbol, access: Accessability, errorString: String, pos: Positioned) =
    access match {
      case Public                                                                                =>
      case Private if classSymbol == currentMethodSymbol.classSymbol                             =>
      case Protected if classSymbol == currentMethodSymbol.classSymbol                           =>
      case Protected if currentMethodSymbol.classSymbol.getType.isSubTypeOf(classSymbol.getType) =>
      case Private | Protected                                                                   =>
        error(errorString, pos)
    }

  def checkConstructorPrivacy(classSymbol: ClassSymbol, methodSymbol: MethodSymbol): Unit = {
    val access = methodSymbol.access
    val accessString = methodSymbol.access.toString.toLowerCase
    val className = classSymbol.name
    val callingClass = currentMethodSymbol.classSymbol.name
    val errorString = s"Cannot call $accessString constructor of '$className' from class '$callingClass'."
    _checkPrivacy(classSymbol, access, errorString, methodSymbol)
  }


  def checkMethodPrivacy(classSymbol: ClassSymbol, methodSymbol: MethodSymbol): Unit = {
    val access = methodSymbol.access
    val accessString = methodSymbol.access.toString.toLowerCase
    val className = classSymbol.name
    val methodName = methodSymbol.name
    val callingClass = currentMethodSymbol.classSymbol.name
    val errorString = s"Cannot call $accessString method '$methodName' in '$className' from class '$callingClass'."
    _checkPrivacy(classSymbol, access, errorString, methodSymbol)
  }

  def checkFieldPrivacy(classSymbol: ClassSymbol, varSymbol: VariableSymbol): Unit = {
    val access = varSymbol.access
    val accessString = varSymbol.access.toString.toLowerCase
    val className = classSymbol.name
    val fieldName = varSymbol.name
    val callingClass = currentMethodSymbol.classSymbol.name
    val errorString = s"Cannot access $accessString field '$fieldName' in '$className' from class '$callingClass'."
    _checkPrivacy(classSymbol, access, errorString, varSymbol)
  }

  def checkOperatorPrivacy(classSymbol: ClassSymbol, operatorSymbol: OperatorSymbol): Unit = {
    val access = operatorSymbol.access
    val accessString = operatorSymbol.access.toString.toLowerCase
    val className = classSymbol.name
    val operatorName = Trees.operatorString(operatorSymbol)
    val callingClass = currentMethodSymbol.classSymbol.name
    val errorString = s"Cannot call $accessString operator '$operatorName' in '$className' from class '$callingClass'."
    _checkPrivacy(classSymbol, access, errorString, operatorSymbol)
  }

  def tcBinaryOperator(expr: ExprTree, args: (Type, Type), expectedType: Option[Type] = None, errorOnNotFound: Boolean = true): Type = {
    val argList = List(args._1, args._2)
    val operatorType = typeCheckOperator(args._1, expr, argList) match {
      case Some(tpe)                  =>
        tpe
      case None if args._1 != args._2 =>
        typeCheckOperator(args._2, expr, argList) match {
          case Some(tpe) => tpe
          case None      => if (errorOnNotFound) operatorError(expr, args) else TUntyped
        }
      case _                          => if (errorOnNotFound) operatorError(expr, args) else TUntyped
    }
    correctOperatorType(expr, argList, expectedType, operatorType)
  }

  def tcUnaryOperator(expr: ExprTree, arg: Type, expectedType: Option[Type] = None): Type = {
    val argList = List(arg)
    val operatorType = typeCheckOperator(arg, expr, argList) match {
      case Some(tpe) => tpe
      case None      => operatorError(expr, arg)
    }
    correctOperatorType(expr, argList, expectedType, operatorType)
  }

  private def typeCheckOperator(classType: Type, expr: ExprTree, args: List[Type], expectedType: Option[Type] = None): Option[Type] =
    classType match {
      case TObject(classSymbol) =>
        classSymbol.lookupOperator(expr, args) match {
          case Some(operatorSymbol) =>
            checkOperatorPrivacy(classSymbol, operatorSymbol)
            expr.setType(operatorSymbol.getType)
            Some(operatorSymbol.getType)
          case None                 => None
        }
      case _                    => None
    }

  private def correctOperatorType(expr: ExprTree, args: List[Type], expectedType: Option[Type], found: Type): Type =
    expectedType match {
      case Some(expected) =>
        if (found != expected) operatorReturnTypeError(expr, args, expected, found)
        else found
      case _              => found
    }

  def operatorError(expr: ExprTree, args: (Type, Type)): Type = operatorError(expr, List(args._1, args._2))
  def operatorError(expr: ExprTree, arg: Type): Type = operatorError(expr, List(arg))
  def operatorError(expr: ExprTree, args: List[Type]): Type = {
    val classesString = if (args.size == 2 && args(0) != args(1))
      "Class " + args.mkString(" or ") + " do"
    else
      "Class " + args(0) + " does"

    error(s"$classesString not contain an operator '${Trees.operatorString(expr, args)}'.", expr)
  }

  def operatorReturnTypeError(expr: ExprTree, args: List[Type], expected: Type, found: Type) =
    if (found == TUntyped)
      found
    else
      error(s"Operator '${Trees.operatorString(expr, args)}' has wrong return type: expected '$expected', found '$found'.", expr)

  def tcStat(stat: StatTree): Unit = stat match {
    case Block(stats)                     =>
      stats.foreach(tcStat)
    case If(expr, thn, els)               =>
      tcExpr(expr, TBool)
      tcStat(thn)
      if (els.isDefined)
        tcStat(els.get)
    case While(expr, stat)                =>
      tcExpr(expr, TBool)
      tcStat(stat)
    case For(init, condition, post, stat) =>
      init.foreach(tcStat(_))
      tcExpr(condition, TBool)
      post.foreach(tcStat)
      tcStat(stat)
    case Print(expr)                      =>
      if (tcExpr(expr) == TUnit) error("Cannot call print with type Unit.", expr)
    case Println(expr)                    =>
      if (tcExpr(expr) == TUnit) error("Cannot call println with type Unit.", expr)
    case Return(Some(expr))               =>
      tcExpr(expr, currentMethodSymbol.getType)
    case r @ Return(None)                 =>
      if (currentMethodSymbol.getType != TUnit) error("Expected a return value of type \'" + currentMethodSymbol.getType + "\'.", r)
    case expr: ExprTree                   =>
      tcExpr(expr)
  }

  def tcExpr(expr: ExprTree, expected: Type*): Type = {
    tcExpr(expr, expected.toList)
  }

  def tcExpr(expression: ExprTree, expected: List[Type]): Type = {
    val tpe = expression match {
      case _: IntLit                    => TInt
      case _: LongLit                   => TLong
      case _: FloatLit                  => TFloat
      case _: DoubleLit                 => TDouble
      case _: CharLit                   => TChar
      case _: StringLit                 => TString
      case _: True                      => TBool
      case _: False                     => TBool
      case id: Identifier               => id.getType
      case id: ClassIdentifier          => id.getType
      case th: This                     => th.getSymbol.getType
      case mc: MethodCall               => typeCheckMethodCall(mc)
      case fr: FieldRead                => typeCheckFieldRead(fr)
      case fa: FieldAssign              => typeCheckFieldAssign(fa)
      case NewArray(tpe, size)          =>
        tcExpr(size, TInt)
        TArray(tpe.getType)
      case Plus(lhs, rhs)               =>
        val types = List(Types.anyObject, TString, TBool) ::: Types.primitives
        val argTypes = (tcExpr(lhs, types), tcExpr(rhs, types))
        argTypes match {
          case (TString, _) | (_, TString)       => TString
          case (_, _: TObject) | (_: TObject, _) => tcBinaryOperator(expression, argTypes)
          case (TBool, x)                        => typeError(x, "Bool", lhs)
          case (x, TBool)                        => typeError(x, "Bool", rhs)
          case (TDouble, _) | (_, TDouble)       => TDouble
          case (TFloat, _) | (_, TFloat)         => TFloat
          case (TLong, _) | (_, TLong)           => TLong
          case (TString, _) | (_, TString)       => operatorError(expression, argTypes)
          case _                                 => TInt
        }
      case BinaryOperator(lhs, rhs)     =>
        val types = List(Types.anyObject, TString) ::: Types.primitives
        val argTypes = (tcExpr(lhs, types), tcExpr(rhs, types))
        argTypes match {
          case (_, _: TObject) | (_: TObject, _) => tcBinaryOperator(expression, argTypes)
          case (TString, _) | (_, TString)       => operatorError(expression, argTypes)
          case (TDouble, _) | (_, TDouble)       => TDouble
          case (TFloat, _) | (_, TFloat)         => TFloat
          case (TLong, _) | (_, TLong)           => TLong
          case _                                 => TInt
        }
      case LogicalOperator(lhs, rhs)    =>
        val types = List(Types.anyObject, TString, TBool, TInt, TLong, TChar)
        val argTypes = (tcExpr(lhs, types), tcExpr(rhs, types))
        argTypes match {
          case (_: TObject, _) | (_, _: TObject) => tcBinaryOperator(expression, argTypes)
          case (TString, _) | (_, TString)       => operatorError(expression, argTypes)
          case (TBool, TBool)                    => TBool
          case (TBool, x)                        => typeError(x, "Bool", lhs)
          case (x, TBool)                        => typeError(x, "Bool", rhs)
          case (TLong, _) | (_, TLong)           => TLong
          case (TInt, _) | (_, TInt)             => TInt
          case (TChar, TChar)                    => TInt
          case (TChar, x)                        => typeError(x, "Char", lhs)
          case (x, TChar)                        => typeError(x, "Char", rhs)
          case _                                 => TError
        }
      case ShiftOperator(lhs, rhs)      =>
        val types = List(Types.anyObject, TString, TInt, TLong, TChar)
        val argTypes = (tcExpr(lhs, types), tcExpr(rhs, types))
        argTypes match {
          case (_: TObject, _) | (_, _: TObject) => tcBinaryOperator(expression, argTypes)
          case (TString, _) | (_, TString)       => operatorError(expression, argTypes)
          case (TLong, _) | (_, TLong)           => TLong
          case _                                 => TInt
        }
      case Assign(id, expr)             =>
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
        }
      case ArrayAssign(id, index, expr) =>
        tcExpr(index, TInt)
        tcExpr(id) match {
          case TArray(arrayTpe) =>
            arrayTpe match {
              case objType: TObject => tcExpr(expr, objType)
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
              case _: TArray        => error("Arrays in arrays are not allowed.", id) // TODO: Support arrays in arrays
            }
          case x                => typeError(id.getType + "[]", x, id)
        }
      case ComparisonOperator(lhs, rhs) =>
        val types = List(Types.anyObject, TString) ::: Types.primitives
        val argTypes = (tcExpr(lhs, types), tcExpr(rhs, types))
        argTypes match {
          case (_: TObject, _) | (_, _: TObject) => tcBinaryOperator(expression, argTypes, Some(TBool))
          case (TString, _) | (_, TString)       => operatorError(expression, argTypes)
          case _                                 =>
        }
        TBool
      case EqualsOperator(lhs, rhs)     =>
        val argTypes @ (lhsType, rhsType) = (tcExpr(lhs), tcExpr(rhs))
        argTypes match {
          case (_: TObject, _) | (_, _: TObject) =>
            tcBinaryOperator(expression, argTypes, Some(TBool), errorOnNotFound = false)
          case _                                 =>
            lhsType match {
              case arrayType: TArray =>
                if (!rhsType.isSubTypeOf(arrayType))
                  typeError(arrayType, rhsType, expression)
              case TString           =>
                if (!rhsType.isSubTypeOf(TString))
                  typeError(TString, rhsType, expression)
              case TBool             =>
                if (!rhsType.isSubTypeOf(TBool))
                  typeError(TBool, rhsType, expression)
              case _                 =>
            }
        }
        TBool
      case And(lhs, rhs)                =>
        tcExpr(lhs, TBool)
        tcExpr(rhs, TBool)
        TBool
      case Or(lhs, rhs)                 =>
        tcExpr(lhs, TBool)
        tcExpr(rhs, TBool)
        TBool
      case Not(expr)                    =>
        tcExpr(expr, TBool, Types.anyObject) match {
          case x: TObject => tcUnaryOperator(expression, x, Some(TBool))
          case _          =>
        }
        TBool
      case Instance(expr, id)           =>
        val tpe = tcExpr(expr)
        tpe match {
          case t: TObject =>
            tcExpr(id, tpe.getSuperTypes)
            TBool
          case _          => typeError("object", tpe, expr)
        }
      case As(expr, tpe)                =>
        tcExpr(expr, tpe.getType.getSuperTypes)
        tpe.getType
      case ArrayRead(arr, index)        =>
        tcExpr(index, TInt)
        tcExpr(arr) match {
          case TArray(arrTpe) => arrTpe
          case x              => typeError("array", x, arr)
        }
      case ArrayLength(arr)             =>
        tcExpr(arr) match {
          case TArray(_) => TInt
          case x         => typeError("array", x, arr)
        }
      case newDecl @ New(tpe, exprs)    =>
        val argTypes = exprs.map(tcExpr(_))

        tpe.getType match {
          case TObject(classSymbol) =>
            classSymbol.lookupMethod(tpe.value, argTypes) match {
              case Some(constructorSymbol) => checkConstructorPrivacy(classSymbol, constructorSymbol)
              case None                    =>
                if (exprs.nonEmpty) {
                  val methodSignature = tpe.value + exprs.map(_.getType).mkString("(", " , ", ")")
                  error("Class \'" + classSymbol.name + "\' does not contain a constructor \'" + methodSignature + "\'.", newDecl)
                }
            }
          case x                    => error("Cannot create a new instance of primitive type \'" + x + "\'.", newDecl)
        }
        tpe.getType
      case Negation(expr)               =>
        tcExpr(expr, Types.anyObject :: Types.primitives) match {
          case x: TObject => tcUnaryOperator(expression, x)
          case TChar      => TInt // Negation of char is int
          case x          => x
        }
      case IncrementDecrement(id)       =>
        tcExpr(id, Types.anyObject :: Types.primitives) match {
          case x: TObject => tcUnaryOperator(expression, x, Some(x)) // Requires same return type as type
          case x          => x
        }
      case LogicNot(expr)               =>
        tcExpr(expr, Types.anyObject, TInt, TLong, TChar) match {
          case x: TObject => tcUnaryOperator(expression, x)
          case TLong      => TLong
          case _          => TInt
        }
      case Ternary(condition, thn, els) =>
        tcExpr(condition, TBool)
        val thenType = tcExpr(thn)
        tcExpr(els, thenType)
    }

    // Check result and return a valid type in case of error
    val res = if (expected.isEmpty) {
      tpe
    } else if (!expected.exists(e => tpe.isSubTypeOf(e))) {
      typeError(makeExpectedString(expected), tpe.toString, expression)
    } else {
      tpe
    }
    expression.setType(res)
    res
  }

  def makeExpectedString(expected: List[Type]): String = expected.size match {
    case 0 => ""
    case 1 => expected.head.toString
    case n => expected.take(n - 1).mkString(", ") + " or " + expected.last
  }

  def errorOr(expr: Type, or: Type) = expr match {
    case TError => TError
    case _      => or
  }

  def typeError(expected: Type, found: Type, pos: Positioned): Type = typeError(expected.toString, found.toString, pos)
  def typeError(expected: String, found: Type, pos: Positioned): Type = typeError(expected, found.toString, pos)
  def typeError(expected: Type, found: String, pos: Positioned): Type = typeError(expected.toString, found, pos)

  def typeError(expected: String, found: String, pos: Positioned): Type =
    error("Type error: expected: \'" + expected + "\', found: \'" + found + "\'.", pos)

  def error(msg: String, pos: Positioned) = {
    ctx.reporter.error(msg, pos)
    TError
  }

  def any(tuple: (Type, Type), tpe: Type) = tuple._1 == tpe || tuple._2 == tpe
}
