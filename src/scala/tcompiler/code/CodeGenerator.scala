package tcompiler.code

import cafebabe.AbstractByteCodes._
import cafebabe.ByteCodes._
import cafebabe.CodeHandler
import tcompiler.analyzer.Symbols._
import tcompiler.analyzer.Types._
import tcompiler.analyzer._
import tcompiler.ast.TreeGroups._
import tcompiler.ast.Trees._
import tcompiler.utils.Extensions._
import scala.collection._


/**
  * Created by Tim Lindeberg on 4/2/2016.
  */
object CodeGenerator {

  val ConstructorName = "<init>"

  val JavaLang = "java/lang/"
  val JavaIO   = "java/io/"

  /* Java classes used by compiler */
  val JavaStringBuilder = JavaLang + "StringBuilder"
  val JavaString        = JavaLang + "String"
  val JavaSystem        = JavaLang + "System"
  val JavaPrintStream   = JavaIO + "PrintStream"
  val JavaObject        = JavaLang + "Object"

  val JavaInt              = JavaLang + "Integer"
  val JavaChar             = JavaLang + "Character"
  val JavaFloat            = JavaLang + "Float"
  val JavaDouble           = JavaLang + "Double"
  val JavaLong             = JavaLang + "Long"
  val JavaRuntimeException = JavaLang + "RuntimeException"


  /* Labels */
  val Then  = "then"
  val Else  = "else"
  val After = "after"
  val Body  = "body"
  val Post  = "post"
  val Next  = "next"

}

class CodeGenerator(ch: CodeHandler, className: String, variableMap: mutable.HashMap[VariableSymbol, Int]) {

  import CodeGenerator._

  def compileStat(statement: StatTree, before: Option[String] = None, after: Option[String] = None): Unit = {
    ch << LineNumber(statement.line)
    statement match {
      case UselessStatement(expr)                      =>
      // Don't compile useless statements
      case Block(stats)                                =>
        stats.foreach(compileStat(_, before, after))
      case v@VarDecl(_, varId, init, modifiers) =>
        val sym = v.getSymbol
        val tpe = sym.getType
        val id = ch.getFreshVar(tpe.size)
        val codes = tpe.codes

        variableMap(sym) = id
        init match {
          case Some(expr) =>
            compileAssignmentValue(expr, tpe)
            codes.store(ch, variableMap(sym))
          case None       =>
            codes.defaultConstant(ch)
            codes.store(ch, id)
        }
      case If(expr, thn, els)                          =>
        val thnLabel = ch.getFreshLabel(Then)
        val elsLabel = ch.getFreshLabel(Else)
        val afterLabel = ch.getFreshLabel(After)

        compileBranch(expr, Label(thnLabel), Label(elsLabel))
        ch << Label(thnLabel)
        compileStat(thn, before, after)
        ch << Goto(afterLabel)
        ch << Label(elsLabel)
        if (els.isDefined) compileStat(els.get, before, after)
        ch << Label(afterLabel)
      case While(expr, stat)                           =>
        val body = ch.getFreshLabel(Body)
        val continue = ch.getFreshLabel(Post)
        val after = ch.getFreshLabel(After)
        val bodyLabel = Label(body)
        val afterLabel = Label(after)
        val continueLabel = Label(continue)

        ch << continueLabel
        compileBranch(expr, bodyLabel, afterLabel)
        ch << bodyLabel
        compileStat(stat, Some(continue), Some(after))
        compileBranch(expr, bodyLabel, afterLabel)
        ch << afterLabel
      case For(init, condition, postExprs, stat)            =>
        val body = ch.getFreshLabel(Body)
        val continue = ch.getFreshLabel(Post)
        val after = ch.getFreshLabel(After)
        val bodyLabel = Label(body)
        val afterLabel = Label(after)
        val continueLabel = Label(continue)

        init.foreach(compileStat(_))
        compileBranch(condition, bodyLabel, afterLabel)
        ch << bodyLabel
        compileStat(stat, Some(continue), Some(after))
        ch << continueLabel
        postExprs.foreach(compileStat(_))
        compileBranch(condition, bodyLabel, afterLabel)
        ch << afterLabel
      case PrintStatement(expr)                        =>
        ch << GetStatic(JavaSystem, "out", "L" + JavaPrintStream + ";")
        compileExpr(expr)
        val arg = expr.getType match {
          case _: TObject => "L" + JavaObject + ";" // Call System.out.println(Object) for all other types
          case _          => expr.getType.byteCodeName
        }
        val funcName = statement match {
          case _: Print   => "print"
          case _: Println => "println"
        }
        ch << InvokeVirtual(JavaPrintStream, funcName, "(" + arg + ")V")
      case Error(expr)                                 =>
        ch << GetStatic(JavaSystem, "out", "L" + JavaPrintStream + ";")
        ch << InvokeVirtual(JavaPrintStream, "flush", "()V")
        ch << cafebabe.AbstractByteCodes.New(JavaRuntimeException) << DUP
        compileExpr(expr)
        ch << InvokeSpecial(JavaRuntimeException, "<init>", "(L" + JavaString + ";)V")
        ch << ATHROW
      case Return(Some(expr))                          =>
        compileExpr(expr)
        expr.getType.codes.ret(ch)
      case Return(None)                                =>
        ch << RETURN
      case Break() =>
        ch << Goto(after.get)
      case Continue() =>
        ch << Goto(before.get)
      case expr: ExprTree                              =>
        // Assignment, method call or increment/decrement
        compileExpr(expr, duplicate = false)
    }
  }

  def compileExpr(expression: ExprTree, duplicate: Boolean = true): Unit = {
    ch << LineNumber(expression.line)
    expression match {
      case True()                                            => ch << Ldc(1)
      case False()                                           => ch << Ldc(0)
      case IntLit(value)                                     => ch << Ldc(value)
      case LongLit(value)                                    => ch << Ldc(value)
      case CharLit(value)                                    => ch << Ldc(value)
      case FloatLit(value)                                   => ch << Ldc(value)
      case DoubleLit(value)                                  => ch << Ldc(value)
      case StringLit(value)                                  => ch << Ldc(value)
      case id: Identifier                                    => load(id)
      case This()                                            => ch << ArgLoad(0)
      case Super()                                           => ch << ArgLoad(0)
      case _: And |
           _: Or |
           _: Equals |
           _: NotEquals |
           _: LessThan |
           _: LessThanEquals |
           _: GreaterThan |
           _: GreaterThanEquals |
           _: Not                                            =>
        val thn = ch.getFreshLabel(Then)
        val els = ch.getFreshLabel(Else)
        val after = ch.getFreshLabel(After)
        compileBranch(expression, Label(thn), Label(els))
        ch << Label(thn)
        ch << Ldc(1)
        ch << Goto(after)
        ch << Label(els)
        ch << Ldc(0)
        ch << Label(after)
      case arrLit@ArrayLit(expressions)                      =>
        compileArrayLiteral(arrLit)
      case newArray@tcompiler.ast.Trees.NewArray(tpe, sizes) =>
        sizes.foreach(compileExpr(_))
        if (newArray.dimension == 1)
          tpe.getType.codes.newArray(ch)
        else
          ch << NewMultidimensionalArray(newArray.getType.byteCodeName, newArray.dimension)
      case Plus(lhs, rhs)                                    =>
        val args = (lhs.getType, rhs.getType)
        args match {
          case _ if args.anyIs(TString)    =>
            if (operatorDefinedFor(expression, args)) {
              compileExpr(lhs)
              compileExpr(rhs)
              compileOperatorCall(ch, expression, args)
              return
            }

            def methSignature(expr: ExprTree) = {
              val arg = expr.getType match {
                case TObject(_) => "L" + JavaObject + ";"
                case _          => expr.getType.byteCodeName
              }
              "(" + arg + ")L" + JavaStringBuilder + ";"
            }

            // TODO: Print array in a nicer way?
            ch << DefaultNew(JavaStringBuilder)
            compileExpr(lhs)
            ch << InvokeVirtual(JavaStringBuilder, "append", methSignature(lhs))
            compileExpr(rhs)
            ch << InvokeVirtual(JavaStringBuilder, "append", methSignature(rhs))
            ch << InvokeVirtual(JavaStringBuilder, "toString", "()L" + JavaString + ";")
          case _ if args.anyIs(tObject) =>
            compileExpr(lhs)
            compileExpr(rhs)
            compileOperatorCall(ch, expression, args)
          case _ if args.anyIs(TDouble)       =>
            compileExpr(lhs)
            lhs.getType.codes.toDouble(ch)
            compileExpr(rhs)
            rhs.getType.codes.toDouble(ch)
            ch << DADD
          case _ if args.anyIs(TFloat)         =>
            compileExpr(lhs)
            lhs.getType.codes.toFloat(ch)
            compileExpr(rhs)
            rhs.getType.codes.toFloat(ch)
            ch << FADD
          case _ if args.anyIs(TLong)           =>
            compileExpr(lhs)
            lhs.getType.codes.toLong(ch)
            compileExpr(rhs)
            rhs.getType.codes.toLong(ch)
            ch << LADD
          case _                                 =>
            compileExpr(lhs)
            lhs.getType.codes.toInt(ch)
            compileExpr(rhs)
            rhs.getType.codes.toInt(ch)
            ch << IADD
        }
      case BinaryOperator(lhs, rhs)                          =>
        compileExpr(lhs)

        val args = (lhs.getType, rhs.getType)
        args match {
          case _ if args.anyIs(tObject) =>
            compileExpr(rhs)
            compileOperatorCall(ch, expression, args)
            return
          case _ if args.anyIs(TDouble)       =>
            lhs.getType.codes.toDouble(ch)
            compileExpr(rhs)
            rhs.getType.codes.toDouble(ch)
          case _ if args.anyIs(TFloat)         =>
            lhs.getType.codes.toFloat(ch)
            compileExpr(rhs)
            rhs.getType.codes.toFloat(ch)
          case _ if args.anyIs(TLong)           =>
            lhs.getType.codes.toLong(ch)
            compileExpr(rhs)
            rhs.getType.codes.toLong(ch)
          case _                                 =>
            lhs.getType.codes.toInt(ch)
            compileExpr(rhs)
            rhs.getType.codes.toInt(ch)
        }
        val codes = expression.getType.codes
        expression match {
          case _: Minus  => codes.sub(ch)
          case _: Times  => codes.mul(ch)
          case _: Div    => codes.div(ch)
          case _: Modulo => codes.mod(ch)
        }
      case LogicalOperator(lhs, rhs)                         =>
        val args = (lhs.getType, rhs.getType)
        args match {
          case _ if args.anyIs(tObject) =>
            compileExpr(lhs)
            compileExpr(rhs)
            compileOperatorCall(ch, expression, args)
            return
          case _ if args.anyIs(TLong)           =>
            compileExpr(lhs)
            lhs.getType.codes.toLong(ch)
            compileExpr(rhs)
            rhs.getType.codes.toLong(ch)
          case _                                 =>
            compileExpr(lhs)
            lhs.getType.codes.toInt(ch)
            compileExpr(rhs)
            rhs.getType.codes.toInt(ch)
        }
        val codes = expression.getType.codes
        expression match {
          case _: LogicAnd => codes.and(ch)
          case _: LogicOr  => codes.or(ch)
          case _: LogicXor => codes.xor(ch)
        }
      case ShiftOperator(lhs, rhs)                           =>
        compileExpr(lhs)

        val args = (lhs.getType, rhs.getType)
        args match {
          case _ if args.anyIs(tObject) =>
            compileExpr(rhs)
            compileOperatorCall(ch, expression, args)
            return
          case (TLong, _)                        =>
            compileExpr(rhs)
            rhs.getType.codes.toInt(ch)
          case (_, TLong)                        =>
            lhs.getType.codes.toLong(ch)
            compileExpr(rhs)
            rhs.getType.codes.toInt(ch)
          case _                                 =>
            lhs.getType.codes.toInt(ch)
            compileExpr(rhs)
            rhs.getType.codes.toInt(ch)
        }
        val codes = expression.getType.codes
        expression match {
          case _: LeftShift  => codes.leftShift(ch)
          case _: RightShift => codes.rightShift(ch)
        }
      case Assign(id, expr)                                  =>
        store(id, () => compileAssignmentValue(expr, id.getType), duplicate)
      case ArrayAssign(arr, index, expr)                     =>
        compileExpr(arr)
        compileExpr(index)


        arr.getType match {
          case obj@TObject(classSymbol) =>
            classSymbol.lookupOperator(expression, List(index.getType, expr.getType)) match {
              case Some(operatorSymbol) =>
                compileAssignmentValue(expr, operatorSymbol.lookupArgument(1).getType) // second argument is value

                if (duplicate)
                  obj.codes.dup_x2(ch) // arrayref index value -> value arrayref index value
              val className = classSymbol.name
                ch << InvokeVirtual(className, operatorSymbol.methodName, operatorSymbol.signature)
              case None                 => ??? // This shouldnt happen
            }
          case TArray(arrayType)        =>
            compileAssignmentValue(expr, arrayType)
            val idCodes = arrayType.codes
            convertType(ch, expr.getType, arrayType)
            if (duplicate)
              idCodes.dup_x2(ch) // arrayref index value -> value arrayref index value
            idCodes.arrayStore(ch)
          case _                        => ???
        }
      case Instance(expr, id)                                =>
        compileExpr(expr)
        ch << InstanceOf(id.value)
      case As(expr, tpe)                                     =>
        if (tpe.getType.isSubTypeOf(expr.getType)) {
          compileExpr(expr)
          ch << CheckCast(tpe.name)
        } else {
          compileAssignmentValue(expr, tpe.getType)
        }
      case ArrayRead(id, index)                              =>
        compileExpr(id)
        compileExpr(index)
        id.getType match {
          case TObject(classSymbol) =>
            classSymbol.lookupOperator(expression, List(index.getType)) match {
              case Some(operatorSymbol) =>
                val className = classSymbol.name
                ch << InvokeVirtual(className, operatorSymbol.methodName, operatorSymbol.signature)
              case None                 => ???
            }
          case TArray(arrayType)    =>
            arrayType.codes.arrayLoad(ch)
          case _                    => ???
        }
      case FieldRead(obj, id)                                =>
        val className = obj.getType.asInstanceOf[TObject].classSymbol.name

        if (isStaticCall(obj) || isStatic(id)) {
          ch << GetStatic(className, id.value, id.getType.byteCodeName)
        } else {
          compileExpr(obj)
          ch << GetField(className, id.value, id.getType.byteCodeName)
        }
      case FieldAssign(obj, id, expr)                        =>
        val fieldType = id.getType
        val static = isStaticCall(obj) || isStatic(id)

        val className = obj.getType.asInstanceOf[TObject].classSymbol.name
        if (!static)
          compileExpr(obj)
        compileExpr(expr)
        convertType(ch, expr.getType, fieldType)
        if (duplicate) {
          if (static) id.getType.codes.dup(ch)
          else id.getType.codes.dup_x1(ch) // ref value -> value ref value
        }
        if (static) ch << PutStatic(className, id.value, id.getType.byteCodeName)
        else ch << PutField(className, id.value, id.getType.byteCodeName)
      case mc@MethodCall(obj, meth, args)                    =>
        obj.getType match {
          case TArray(tpe) =>
            assert(meth.value == "Size" && args.isEmpty)
            compileExpr(obj)
            ch << ARRAYLENGTH
          case TObject(classSymbol) =>
            val className = classSymbol.name
            val methSymbol = meth.getSymbol.asInstanceOf[MethodSymbol]
            val signature = methSymbol.signature

            def compileArgs() = methSymbol.argList.zip(args).foreach {
              case (expected, givenExpr) => compileAssignmentValue(givenExpr, expected.getType)
            }

            if (isStaticCall(obj) || isStatic(meth)) {
              compileArgs()
              ch << InvokeStatic(className, meth.value, signature)
            } else {
              compileExpr(obj)
              compileArgs()
              if(obj.isInstanceOf[Super])
                ch << InvokeSpecial(className, meth.value, signature)
              else
                ch << InvokeVirtual(className, meth.value, signature)
            }

            if (!duplicate && mc.getType != TUnit)
              ch << POP
          case _ => ???
        }
      case tcompiler.ast.Trees.New(tpe, args)                =>
        tpe.getType match {
          case TObject(classSymbol) =>
            val codes = tpe.getType.codes
            val objName = if (tpe.name == "Object") JavaObject else tpe.name
            ch << cafebabe.AbstractByteCodes.New(objName)
            codes.dup(ch)
            args.foreach(compileExpr(_))

            val signature = "(" + args.map(_.getType.byteCodeName).mkString + ")V"
            ch << InvokeSpecial(objName, ConstructorName, signature)
          case primitiveType        =>
            if (args.size == 1) {
              compileExpr(args.head)
              convertType(ch, args.head.getType, primitiveType)
            }
            else {
              primitiveType.codes.defaultConstant(ch)
            }
        }

      case Negation(expr)               =>
        compileExpr(expr)
        expr.getType match {
          case x: TObject => compileOperatorCall(ch, expression, x)
          case _          => expr.getType.codes.negation(ch)
        }
      case LogicNot(expr)               =>
        compileExpr(expr)
        expr.getType match {
          case x: TObject => compileOperatorCall(ch, expression, x)
          case _          =>
            ch << Ldc(-1)
            expr.getType.codes.xor(ch)
        }
      case Hash(expr)                   =>

        def hashFunction(className: String) = {
          ch << cafebabe.AbstractByteCodes.New(className) << DUP
          compileExpr(expr)
          ch << InvokeSpecial(className, ConstructorName, "(" + expr.getType.byteCodeName + ")V") <<
            InvokeVirtual(className, "hashCode", "()I")
        }

        expr.getType match {
          case x: TObject =>
            compileExpr(expr)
            if (!compileOperatorCall(ch, expression, x))
              ch << InvokeVirtual(JavaObject, "hashCode", "()I")
          case TString    => hashFunction(JavaString)
          case TInt       => hashFunction(JavaInt)
          case TChar      => hashFunction(JavaChar)
          case TFloat     => hashFunction(JavaFloat)
          case TDouble    => hashFunction(JavaDouble)
          case TLong      => hashFunction(JavaLong)
          case _          => ???
        }
      case PreIncrement(expr)           => compileIncrementDecrement(isPre = true, isIncrement = true, expr)
      case PreDecrement(expr)           => compileIncrementDecrement(isPre = true, isIncrement = false, expr)
      case PostIncrement(expr)          => compileIncrementDecrement(isPre = false, isIncrement = true, expr)
      case PostDecrement(expr)          => compileIncrementDecrement(isPre = false, isIncrement = false, expr)
      case Ternary(condition, thn, els) =>
        val thnLabel = ch.getFreshLabel(Then)
        val elsLabel = ch.getFreshLabel(Else)
        val afterLabel = ch.getFreshLabel(After)

        compileBranch(condition, Label(thnLabel), Label(elsLabel))
        ch << Label(thnLabel)
        compileExpr(thn)
        ch << Goto(afterLabel)
        ch << Label(elsLabel)
        compileExpr(els)
        ch << Label(afterLabel)
    }

    def compileIncrementDecrement(isPre: Boolean, isIncrement: Boolean, expr: ExprTree) = {
      expr match {
        case id: Identifier           =>
          val varSymbol = id.getSymbol.asInstanceOf[VariableSymbol]
          val isLocal = variableMap.contains(varSymbol)
          val codes = id.getType.codes
          expr.getType match {
            case TInt if isLocal =>
              if (!isPre && duplicate) compileExpr(expr)

              ch << IInc(variableMap(varSymbol), if (isIncrement) 1 else -1)

              if (isPre && duplicate) compileExpr(expr)
            case x: TObject      =>
              // TODO: Fix post/pre increment for objects. Currently they work the same way.
              store(id, () => {
                load(id)
                compileOperatorCall(ch, expression, x)
              }, duplicate)
            case _               =>
              store(id, () => {
                load(id)
                if (!isPre && duplicate) {
                  if (isLocal) codes.dup(ch)
                  else codes.dup_x1(ch)
                }
                codes.one(ch)
                if (isIncrement) codes.add(ch) else codes.sub(ch)
              }, duplicate && isPre)
          }
        case fr@FieldRead(obj, id)    =>
          if (!isPre && duplicate)
            compileExpr(fr) // TODO: Use dup instead of compiling the whole expression
        val assign = assignExpr(fr, isIncrement)
          compileExpr(FieldAssign(obj, id, assign), isPre && duplicate)
        case ar@ArrayRead(arr, index) =>
          if (!isPre && duplicate)
            compileExpr(ar) // TODO: Use dup instead of compiling the whole expression

          val assign = assignExpr(ar, isIncrement)
          compileExpr(ArrayAssign(arr, index, assign), isPre && duplicate)
      }

      def assignExpr(expr: ExprTree, isIncrement: Boolean) = {
        val tpe = expr.getType

        val o = one(tpe)
        (if (isIncrement) Plus(expr, o) else Minus(expr, o)).setType(tpe)
      }
    }
  }

  private def convertType(ch: CodeHandler, toConvert: Type, desiredType: Type) = {
    val codes = toConvert.codes
    desiredType match {
      case _: TObject | TString =>
      case TDouble              => codes.toDouble(ch)
      case TFloat               => codes.toFloat(ch)
      case TLong                => codes.toLong(ch)
      case _                    => codes.toInt(ch)
    }
  }

  private def one(tpe: Type): ExprTree = {
    val t = tpe match {
      case TInt    => IntLit(1)
      case TChar   => IntLit(1)
      case TLong   => LongLit(1l)
      case TFloat  => FloatLit(1.0f)
      case TDouble => DoubleLit(1.0)
      case _       => ???
    }
    t.setType(tpe)
  }

  private def operatorDefinedFor(expr: ExprTree, args: (Type, Type)): Boolean = {
    val argList = List(args._1, args._2)
    operatorDefinedFor(args._1, expr, argList) || operatorDefinedFor(args._2, expr, argList)
  }

  private def operatorDefinedFor(classType: Type, expr: ExprTree, args: List[Type]): Boolean =
    classType match {
      case TObject(classSymbol) =>
        classSymbol.lookupOperator(expr, args) match {
          case Some(operatorSymbol) => true
          case None                 => false
        }
      case _                    => false
    }

  private def compileOperatorCall(ch: CodeHandler, expr: ExprTree, args: (Type, Type)): Boolean = {
    val argsList = List(args._1, args._2)
    compileOperatorCall(ch, args._1, expr, argsList) || compileOperatorCall(ch, args._2, expr, argsList)
  }

  private def compileOperatorCall(ch: CodeHandler, expr: ExprTree, arg: Type): Boolean =
    compileOperatorCall(ch, arg, expr, List(arg))

  private def compileOperatorCall(ch: CodeHandler, classType: Type, expr: ExprTree, args: List[Type]): Boolean =
    classType match {
      case TObject(classSymbol) =>
        classSymbol.lookupOperator(expr, args) match {
          case Some(operatorSymbol) =>
            ch << InvokeStatic(classSymbol.name, operatorSymbol.methodName, operatorSymbol.signature)
            true
          case None                 => false
        }
      case _                    => false
    }

  private def compileAssignmentValue(expr: ExprTree, desiredType: Type): Unit = {
    if (expr.getType.isSubTypeOf(desiredType)) {
      compileExpr(expr)
      return
    }

    desiredType match {
      case tpe: TObject =>
        // Object has to be placed on stack before argument
        val name = tpe.classSymbol.name
        val objName = if (name == "Object") JavaObject else name
        ch << cafebabe.AbstractByteCodes.New(objName)
        desiredType.codes.dup(ch)
        compileExpr(expr)
        val signature = "(" + expr.getType.byteCodeName + ")V"
        ch << InvokeSpecial(objName, ConstructorName, signature)
      case arr: TArray  =>
        // TODO: Safe cast?
        compileArrayLiteral(expr.asInstanceOf[ArrayLit], Some(arr.tpe))
      case _            =>
        compileExpr(expr)
        convertType(ch, expr.getType, desiredType)
    }
  }

  private def compileArrayLiteral(arrLit: ArrayLit, desiredType: Option[Type] = None) = {
    val expressions = arrLit.expressions
    val arrayType = arrLit.getType.asInstanceOf[TArray].tpe
    val newType = desiredType match {
      case Some(tpe) => tpe
      case None      => arrayType
    }

    ch << Ldc(arrLit.expressions.size)
    newType.codes.newArray(ch)

    expressions.zipWithIndex.foreach { case (expr, i) =>
      arrLit.getType.codes.dup(ch)
      ch << Ldc(i)
      compileAssignmentValue(expr, newType)
      newType.codes.arrayStore(ch)
    }
  }

  private def compileBranch(expression: ExprTree, thn: Label, els: Label): Unit = expression match {
    case Not(expr)                    =>
      expr.getType match {
        case x: TObject =>
          compileExpr(expr)
          compileOperatorCall(ch, expression, x)
          ch << IfEq(els.id) << Goto(thn.id)
        case _          =>
          compileBranch(expr, els, thn)
      }
    case True()                       => ch << Goto(thn.id)
    case False()                      => ch << Goto(els.id)
    case And(lhs, rhs)                =>
      val next = Label(ch.getFreshLabel(Next))
      compileBranch(lhs, next, els)
      ch << next
      compileBranch(rhs, thn, els)
    case Or(lhs, rhs)                 =>
      val next = Label(ch.getFreshLabel(Next))
      compileBranch(lhs, thn, next)
      ch << next
      compileBranch(rhs, thn, els)
    case id@Identifier(value)         =>
      load(id)
      ch << IfEq(els.id) << Goto(thn.id)
    case Instance(expr, id)           =>
      compileExpr(expr)
      ch << Ldc(1)
      ch << InstanceOf(id.value) << If_ICmpEq(thn.id) << Goto(els.id)
    case ComparisonOperator(lhs, rhs) =>
      def comparison(codes: CodeMap) = {
        expression match {
          case _: LessThan          => codes.cmpLt(ch, thn.id)
          case _: LessThanEquals    => codes.cmpLe(ch, thn.id)
          case _: GreaterThan       => codes.cmpGt(ch, thn.id)
          case _: GreaterThanEquals => codes.cmpGe(ch, thn.id)
        }
      }
      val argTypes = (lhs.getType, rhs.getType)
      compileExpr(lhs)
      argTypes match {
        case (_: TObject, _) | (_, _: TObject) =>
          compileExpr(rhs)
          compileOperatorCall(ch, expression, argTypes)
          ch << IfNe(thn.id)
        case (TDouble, _) | (_, TDouble)       =>
          lhs.getType.codes.toDouble(ch)
          compileExpr(rhs)
          rhs.getType.codes.toDouble(ch)
          comparison(DoubleCodeMap)
        case (TFloat, _) | (_, TFloat)         =>
          lhs.getType.codes.toFloat(ch)
          compileExpr(rhs)
          rhs.getType.codes.toFloat(ch)
          comparison(FloatCodeMap)
        case (TLong, _) | (_, TLong)           =>
          lhs.getType.codes.toLong(ch)
          compileExpr(rhs)
          rhs.getType.codes.toLong(ch)
          comparison(LongCodeMap)
        case _                                 =>
          lhs.getType.codes.toInt(ch)
          compileExpr(rhs)
          rhs.getType.codes.toInt(ch)
          comparison(IntCodeMap)
      }

      ch << Goto(els.id)
    case EqualsOperator(lhs, rhs)     =>
      def comparison(codes: CodeMap) = {
        expression match {
          case _: Equals    => codes.cmpEq(ch, thn.id)
          case _: NotEquals => codes.cmpNe(ch, thn.id)
        }
      }

      val argTypes = (lhs.getType, rhs.getType)
      compileExpr(lhs)
      argTypes match {
        case (_, objectType: TObject)    =>
          compileExpr(rhs)
          if (compileOperatorCall(ch, expression, argTypes))
            ch << IfNe(thn.id)
          else
            comparison(objectType.codes) // If no operator is defined, compare by reference
        case (objectType: TObject, _)    =>
          compileExpr(rhs)
          if (compileOperatorCall(ch, expression, argTypes))
            ch << IfEq(thn.id)
          else
            comparison(objectType.codes) // If no operator is defined, compare by reference
        case (_, arrType: TArray)        =>
          compileExpr(rhs)
          comparison(arrType.codes)
        case (arrType: TArray, _)        =>
          compileExpr(rhs)
          comparison(arrType.codes)
        case (TString, _) | (_, TString) =>
          compileExpr(rhs)
          comparison(StringCodeMap)
        case (TDouble, _) | (_, TDouble) =>
          lhs.getType.codes.toDouble(ch)
          compileExpr(rhs)
          rhs.getType.codes.toDouble(ch)
          comparison(DoubleCodeMap)
        case (TFloat, _) | (_, TFloat)   =>
          lhs.getType.codes.toFloat(ch)
          compileExpr(rhs)
          rhs.getType.codes.toFloat(ch)
          comparison(FloatCodeMap)
        case (TLong, _) | (_, TLong)     =>
          lhs.getType.codes.toLong(ch)
          compileExpr(rhs)
          rhs.getType.codes.toLong(ch)
          comparison(LongCodeMap)
        case _                           =>
          lhs.getType.codes.toInt(ch)
          compileExpr(rhs)
          rhs.getType.codes.toInt(ch)
          comparison(IntCodeMap)
      }
      ch << Goto(els.id)
    case mc: MethodCall               =>
      compileExpr(mc)
      ch << IfEq(els.id) << Goto(thn.id)
    case fr: FieldRead                =>
      compileExpr(fr)
      ch << IfEq(els.id) << Goto(thn.id)
  }

  private def store(id: Identifier, put: () => Unit, duplicate: Boolean = false): CodeHandler = {
    val sym = id.getSymbol.asInstanceOf[VariableSymbol]
    val name = id.value
    val tp = id.getType
    val codes = tp.codes

    if (variableMap.contains(sym)) {
      val id = variableMap(sym)
      put()
      if (duplicate)
        codes.dup(ch)
      tp.codes.store(ch, id)
    } else {
      if (isStatic(id)) {
        put()
        if (duplicate)
          codes.dup_x1(ch) // this value -> value this value
        ch << PutStatic(className, name, tp.byteCodeName)
      } else {
        ch << ArgLoad(0) // put this reference on stack
        put()
        if (duplicate)
          codes.dup_x1(ch) // this value -> value this value
        ch << PutField(className, name, tp.byteCodeName)
      }
    }
  }

  private def load(id: Identifier): CodeHandler = {
    val sym = id.getSymbol.asInstanceOf[VariableSymbol]
    val name = id.value
    val tpe = id.getType
    if (variableMap.contains(sym)) {
      val id = variableMap(sym)
      tpe.codes.load(ch, id)
    } else {
      if (isStatic(id)) {
        ch << GetStatic(className, name, tpe.byteCodeName)
      } else {
        ch << ArgLoad(0) // this reference
        ch << GetField(className, name, tpe.byteCodeName)
      }
    }
  }

  private def isStatic(id: Identifier) = id.getSymbol match {
    case m: Modifiable if m.isStatic => true
    case _                           => false
  }

}