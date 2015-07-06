package tcompiler
package analyzer

import tcompiler.analyzer.Symbols.{MethodSymbol, ClassSymbol}
import tcompiler.analyzer.Types._
import tcompiler.ast.Trees._
import tcompiler.utils._
import tcompiler.ast.Printer

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
          case VarDecl(tpe, id, Some(expr)) => typeChecker.tcExpr(expr, tpe.getType)
          case _                            =>
        }
      }
      classDecl.methods.foreach { method =>
        val typeChecker = new TypeChecker(ctx, method.getSymbol)
        method.vars.foreach {
          case VarDecl(tpe, id, Some(expr)) => typeChecker.tcExpr(expr, tpe.getType)
          case _                            =>
        }
        method.stats.foreach(typeChecker.tcStat)
      }
    }

    prog
  }
}

class TypeChecker(ctx: Context, globalMethodSymbol: MethodSymbol) {
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
            checkPrivacy(classSymbol, methSymbol)
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

  def checkPrivacy(classSymbol: ClassSymbol, methodSymbol: MethodSymbol): Unit = methodSymbol.access match {
    case Public                                                                               =>
    case Private if classSymbol == globalMethodSymbol.classSymbol                             =>
    case Protected if classSymbol == globalMethodSymbol.classSymbol                           =>
    case Protected if globalMethodSymbol.classSymbol.getType.isSubTypeOf(classSymbol.getType) =>
    case Private                                                                              =>
      error("Cannot call private method \'" + methodSymbol.name + "\' in \'" + classSymbol.name + "\' from class \'" + globalMethodSymbol.classSymbol.name + "\'.", methodSymbol)
    case Protected                                                                            =>
      error("Cannot call protected method \'" + methodSymbol.name + "\' in \'" + classSymbol.name + "\' from class \'" + globalMethodSymbol.classSymbol.name + "\'.", methodSymbol)
  }

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
      if (tcExpr(expr) == TUnit) error("Cannot call print with type Unit!", expr)
    case Println(expr)                    =>
      if (tcExpr(expr) == TUnit) error("Cannot call println with type Unit!", expr)
    case Return(Some(expr))               =>
      tcExpr(expr, globalMethodSymbol.getType)
    case r @ Return(None)                 =>
      if (globalMethodSymbol.getType != TUnit) error("Expected a return value of type \'" + globalMethodSymbol.getType + "\'.", r)
    case expr: ExprTree                   =>
      tcExpr(expr)
  }

  def tcExpr(expr: ExprTree, expected: Type*): Type = {
    tcExpr(expr, expected.toList)
  }

  def tcExpr(expr: ExprTree, expected: List[Type]): Type = {
    val tpe = expr match {
      case _: IntLit                           => TInt
      case _: LongLit                          => TLong
      case _: FloatLit                         => TFloat
      case _: DoubleLit                        => TDouble
      case _: CharLit                          => TChar
      case _: StringLit                        => TString
      case _: True                             => TBool
      case _: False                            => TBool
      case id: Identifier                      => id.getType
      case id: ClassIdentifier                 => id.getType
      case th: This                            => th.getSymbol.getType
      case mc: MethodCall                      => typeCheckMethodCall(mc)
      case NewArray(tpe, size)                 =>
        tcExpr(size, TInt)
        TArray(tpe.getType)
      case Plus(lhs, rhs)                      =>
        val types = Types.anyObject :: TString :: TBool :: Types.primitives
        (tcExpr(lhs, types), tcExpr(rhs, types)) match {
          case (TString, _) | (_, TString) => TString
          case (TBool, x)                  => typeError(x, "Bool", lhs)
          case (x, TBool)                  => typeError(x, "Bool", rhs)
          case (tpe: TObject, x)           => typeError(x, tpe, lhs)
          case (x, tpe: TObject)           => typeError(x, tpe, rhs)
          case (TDouble, _) | (_, TDouble) => TDouble
          case (TFloat, _) | (_, TFloat)   => TFloat
          case (TLong, _) | (_, TLong)     => TLong
          case _                           => TInt
        }
      case BinaryOperator(lhs, rhs)            =>
        (tcExpr(lhs, Types.primitives), tcExpr(rhs, Types.primitives)) match {
          case (TDouble, _) | (_, TDouble) => TDouble
          case (TFloat, _) | (_, TFloat)   => TFloat
          case (TLong, _) | (_, TLong)     => TLong
          case _                           => TInt
        }
      case LogicalOperator(lhs, rhs)           =>
        val types = List(TBool, TInt, TLong, TChar)
        (tcExpr(lhs, types), tcExpr(rhs, types)) match {
          case (TBool, TBool)          => TBool
          case (TBool, x)              => typeError(x, "Bool", lhs)
          case (x, TBool)              => typeError(x, "Bool", rhs)
          case (TLong, _) | (_, TLong) => TLong
          case (TInt, _) | (_, TInt)   => TInt
          case (TChar, TChar)          => TInt
          case (TChar, x)              => typeError(x, "Char", lhs)
          case (x, TChar)              => typeError(x, "Char", rhs)
          case _                       => TError
        }
      case ShiftOperator(lhs, rhs)             =>
        val types = List(TInt, TLong, TChar)
        (tcExpr(lhs, types), tcExpr(rhs, types)) match {
          case (TLong, _) | (_, TLong) => TLong
          case _                       => TInt
        }
      case Assign(id, expr)                    =>
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
      case PlusAssign(id, expr)                =>
        id.getType match {
          case TString =>
            tcExpr(expr, TString, TInt, TLong, TFloat, TDouble, TChar)
            TString
          case TInt    =>
            tcExpr(expr, TInt, TChar)
            TInt
          case TLong   =>
            tcExpr(expr, TLong, TInt, TChar)
            TLong
          case TFloat  =>
            tcExpr(expr, TFloat, TLong, TInt, TChar)
            TFloat
          case TDouble =>
            tcExpr(expr, TDouble, TFloat, TLong, TInt, TChar)
            TDouble
          case _       => error("Invalid assignment for type \'" + id.getType + "\'.", id)
        }
      case AssignmentOperator(id, expr)        =>
        id.getType match {
          case TInt    =>
            tcExpr(expr, TInt, TChar)
            TInt
          case TLong   =>
            tcExpr(expr, TLong, TInt, TChar)
            TLong
          case TFloat  =>
            tcExpr(expr, TFloat, TLong, TInt, TChar)
            TFloat
          case TDouble =>
            tcExpr(expr, TDouble, TFloat, TLong, TInt, TChar)
            TDouble
          case _       => error("Invalid assignment for type \'" + id.getType + "\'.", id)
        }
      case LogicalAssignmentOperator(id, expr) =>
        id.getType match {
          case TInt  =>
            tcExpr(expr, TInt, TChar)
            TInt
          case TLong =>
            tcExpr(expr, TLong, TInt, TChar)
            TLong
          case TBool => tcExpr(expr, TBool)
          case _     => error("Invalid assignment for type \'" + id.getType + "\'.", id)
        }
      case ShiftAssignmentOperator(id, expr)   =>
        id.getType match {
          case TInt  =>
            tcExpr(expr, TInt, TChar)
            TInt
          case TLong =>
            tcExpr(expr, TLong, TInt, TChar)
            TLong
          case _     => error("Invalid assignment for type \'" + id.getType + "\'.", id)
        }
      case ArrayAssign(id, index, expr)        =>
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
      case ComparisonOperator(lhs, rhs)        =>
        tcExpr(lhs, Types.primitives)
        tcExpr(rhs, Types.primitives)
        TBool
      case EqualsOperator(lhs, rhs)            =>
        tcExpr(lhs) match {
          case tpe: TObject      => tcExpr(rhs, tpe.getSuperTypes)
          case arrayType: TArray => tcExpr(rhs, arrayType)
          case TString           => tcExpr(rhs, TString)
          case TBool             => tcExpr(rhs, TBool)
          case _                 => tcExpr(rhs, Types.primitives)
        }
        TBool
      case And(lhs, rhs)                       =>
        tcExpr(lhs, TBool)
        tcExpr(rhs, TBool)
        TBool
      case Or(lhs, rhs)                        =>
        tcExpr(lhs, TBool)
        tcExpr(rhs, TBool)
        TBool
      case Not(expr)                           =>
        tcExpr(expr, TBool)
        TBool
      case Instance(expr, id)                  =>
        val tpe = tcExpr(expr)
        tpe match {
          case t: TObject =>
            tcExpr(id, tpe.getSuperTypes)
            TBool
          case _          => typeError("object", tpe, expr)
        }
      case As(expr, tpe)                       =>
        tcExpr(expr, tpe.getType.getSuperTypes)
        tpe.getType
      case ArrayRead(arr, index)               =>
        tcExpr(index, TInt)
        tcExpr(arr) match {
          case TArray(arrTpe) => arrTpe
          case x              => typeError("array", x, arr)
        }
      case ArrayLength(arr)                    =>
        tcExpr(arr) match {
          case TArray(_) => TInt
          case x         => typeError("array", x, arr)
        }
      case newDecl @ New(tpe, exprs)           =>
        val argTypes = exprs.map(tcExpr(_))

        tpe.getType match {
          case TObject(classSymbol) =>
            classSymbol.lookupMethod(tpe.value, argTypes) match {
              case Some(constructorSymbol) => checkPrivacy(classSymbol, constructorSymbol)
              case None                    =>
                if (exprs.nonEmpty) {
                  val methodSignature = tpe.value + exprs.map(_.getType).mkString("(", " , ", ")")
                  error("Class \'" + classSymbol.name + "\' does not contain a constructor \'" + methodSignature + "\'.", newDecl)
                }
            }
          case x                    => error("Cannot create a new instance of primitive type \'" + x + "\'.", newDecl)
        }
        tpe.getType

      case Negation(expr)                      =>
        tcExpr(expr, Types.primitives) match {
          case TChar => TInt // Negation of char is int
          case x     => x
        }
      case IncrementDecrement(id)              =>
        tcExpr(id, TChar, TInt, TLong, TFloat, TDouble)
      case LogicNot(expr)                      =>
        tcExpr(expr, TInt, TLong, TChar) match {
          case TLong => TLong
          case _     => TInt
        }
      case Ternary(condition, thn, els)        =>
        tcExpr(condition, TBool)
        val thenType = tcExpr(thn)
        tcExpr(els, thenType)
    }

    // Check result and return a valid type in case of error
    val res = if (expected.isEmpty) {
      tpe
    } else if (!expected.exists(e => tpe.isSubTypeOf(e))) {
      typeError(makeExpectedString(expected), tpe.toString, expr)
    } else {
      tpe
    }
    expr.setType(res)
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
}
