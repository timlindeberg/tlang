package tcompiler.code

import cafebabe.AbstractByteCodes.{InvokeVirtual, _}
import cafebabe.ByteCodes._
import cafebabe.ClassFileTypes._
import cafebabe.CodeHandler
import cafebabe.Flags._
import tcompiler.analyzer.Symbols._
import tcompiler.analyzer.Types._
import tcompiler.analyzer._
import tcompiler.ast.Trees
import tcompiler.ast.Trees._
import tcompiler.utils.Extensions._

import scala.collection.mutable

/**
  * Created by Tim Lindeberg on 4/2/2016.
  */
object CodeGenerator {

  val TraitFlags: U2 = CLASS_ACC_ABSTRACT | CLASS_ACC_PUBLIC | CLASS_ACC_INTERFACE
  val ClassFlags: U2 = CLASS_ACC_PUBLIC

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
  val JavaBool             = JavaLang + "Boolean"
  val JavaRuntimeException = JavaLang + "RuntimeException"

}

class CodeGenerator(ch: CodeHandler, localVariableMap: mutable.HashMap[VariableSymbol, Int]) {

  import CodeGenerator._

  def compileStat(statement: StatTree,
    continue: Option[String] = None,
    break: Option[String] = None,
    compileUseless: Boolean = false): Unit = {
    ch << LineNumber(statement.line)
    statement match {
      case UselessStatement(expr)                    =>
        if (compileUseless)
          compileExpr(expr)
      case Block(stats)                              =>
        stats.foreach(compileStat(_, continue, break, compileUseless))
      case v@VarDecl(_, _, init, _)                  =>
        val sym = v.getSymbol
        val tpe = sym.getType
        val id = ch.getFreshVar(tpe.size)

        localVariableMap(sym) = id
        init ifDefined { expr =>
          compileAndConvert(expr, tpe)
          tpe.codes.store(ch, id)
        }
      case If(conditionExpr, thnStat, elsStat)       =>
        val thn = ch.getFreshLabel("then")
        val els = ch.getFreshLabel("else")
        val after = ch.getFreshLabel("after")

        compileBranch(conditionExpr, Label(thn), Label(els))
        ch << Label(thn)
        compileStat(thnStat, continue, break, compileUseless)
        if (elsStat.isDefined)
          ch << Goto(after)
        ch << Label(els)
        if (elsStat.isDefined)
          compileStat(elsStat.get, continue, break, compileUseless)
        ch << Label(after)
      case While(conditionExpr, stat)                =>
        val body = ch.getFreshLabel("body")
        val condition = ch.getFreshLabel("condition")
        val after = ch.getFreshLabel("after")

        ch << Goto(condition)
        ch << Label(body)
        compileStat(stat, Some(condition), Some(after), compileUseless)
        ch << Label(condition)
        compileBranch(conditionExpr, Label(body), Label(after))
        ch << Label(after)
      case For(init, conditionExpr, postExprs, stat) =>
        val body = ch.getFreshLabel("body")
        val condition = ch.getFreshLabel("condition")
        val after = ch.getFreshLabel("after")
        val post = ch.getFreshLabel("post")

        init.foreach(stat => compileStat(stat, compileUseless = compileUseless))
        ch << Goto(condition)
        ch << Label(body)
        compileStat(stat, Some(post), Some(after), compileUseless)
        ch << Label(post)
        postExprs.foreach(expr => compileStat(expr, compileUseless = compileUseless))
        ch << Label(condition)
        compileBranch(conditionExpr, Label(body), Label(after))
        ch << Label(after)
      case PrintStatTree(expr)                       =>
        ch << GetStatic(JavaSystem, "out", "L" + JavaPrintStream + ";")
        compileExpr(expr)
        val arg = expr.getType match {
          case o: TObject               =>
            if (o.classSymbol.name != JavaString) {
              // First convert to Kool String
              val stringName = Types.String.classSymbol.name
              val stringByteCodeName = Types.String.byteCodeName
              ch << InvokeVirtual(o.classSymbol.name, "ToString", s"()$stringByteCodeName")
              ch << InvokeVirtual(stringName, "toString", s"()L$JavaString;")
            }
            s"L$JavaString;" // Call System.out.println(String) for all other types
          case primitive: PrimitiveType =>
            if (primitive.isNullable) s"L$JavaObject;"
            else primitive.byteCodeName
        }
        val funcName = statement match {
          case _: Print   => "print"
          case _: Println => "println"
        }
        ch << InvokeVirtual(JavaPrintStream, funcName, s"($arg)V")
      case Error(expr)                               =>
        ch << GetStatic(JavaSystem, "out", s"L$JavaPrintStream;")
        ch << InvokeVirtual(JavaPrintStream, "flush", "()V")
        ch << cafebabe.AbstractByteCodes.New(JavaRuntimeException) << DUP
        compileExpr(expr)
        // Convert TString to JavaString
        val stringName = Types.String.classSymbol.name
        ch << InvokeVirtual(stringName, "toString", s"()L$JavaString;")
        ch << InvokeSpecial(JavaRuntimeException, "<init>", s"(L$JavaString;)V")
        ch << ATHROW
      case r@Return(Some(expr))                      =>
        val retType = r.getType
        compileAndConvert(expr, retType)
        retType.codes.ret(ch)
      case Return(None)                              =>
        ch << RETURN
      case Break()                                   =>
        ch << Goto(break.get)
      case Continue()                                =>
        ch << Goto(continue.get)
      case expr: ExprTree                            =>
        compileExpr(expr, duplicate = false)
    }
  }

  def compileExpr(expression: ExprTree, duplicate: Boolean = true): Unit = {
    if (expression.hasPosition)
      ch << LineNumber(expression.line)
    expression match {
      case TrueLit()                                    => ch << Ldc(1)
      case FalseLit()                                   => ch << Ldc(0)
      case NullLit()                                    => ch << ACONST_NULL
      //case Literal(value)                               => ch << Ldc(value)
      case IntLit(value)                                => ch << Ldc(value)
      case LongLit(value)                               => ch << Ldc(value)
      case CharLit(value)                               => ch << Ldc(value)
      case FloatLit(value)                              => ch << Ldc(value)
      case DoubleLit(value)                             => ch << Ldc(value)
      case StringLit(value)                             =>
        val stringName = String.classSymbol.name
        ch << cafebabe.AbstractByteCodes.New(stringName)
        String.codes.dup(ch)
        ch << Ldc(value)
        ch << InvokeSpecial(stringName, ConstructorName, s"(L$JavaString;)V")
      case id: VariableID                               => load(id.getSymbol)
      case _: This                                      => ch << ArgLoad(0)
      case _: Super                                     => ch << ArgLoad(0)
      case _: BranchingOperatorTree                     =>
        val thn = ch.getFreshLabel("then")
        val els = ch.getFreshLabel("else")
        val after = ch.getFreshLabel("after")
        compileBranch(expression, Label(thn), Label(els))
        ch << Label(thn)
        ch << Ldc(1)
        ch << Goto(after)
        ch << Label(els)
        ch << Ldc(0)
        ch << Label(after)
      case arrLit@ArrayLit(expressions)                 =>
        compileArrayLiteral(arrLit)
      case newArray@Trees.NewArray(tpe, sizes)          =>
        sizes foreach (compileExpr(_))
        val dimension = newArray.dimension
        val arrType = tpe.getType.asInstanceOf[TArray].tpe
        if (dimension == 1)
          arrType.codes.newArray(ch)
        else
          ch << NewMultidimensionalArray(tpe.getType.byteCodeName, dimension)
      case arOp@ArithmeticOperatorTree(lhs, rhs)        =>
        val args = (lhs.getType, rhs.getType)
        val desiredType = args match {
          case _ if args.anyIs(Double) => Double
          case _ if args.anyIs(Float)  => Float
          case _ if args.anyIs(Long)   => Long
          case _                       => Int
        }

        compileAndConvert(lhs, desiredType)
        compileAndConvert(rhs, desiredType)
        val codes = desiredType.codes
        expression match {
          case _: Plus   => codes.add(ch)
          case _: Minus  => codes.sub(ch)
          case _: Times  => codes.mul(ch)
          case _: Div    => codes.div(ch)
          case _: Modulo => codes.mod(ch)
        }
      case logicOp@LogicalOperatorTree(lhs, rhs)        =>
        val args = (lhs.getType, rhs.getType)
        val desiredType = args match {
          case _ if args.anyIs(Long) => Long
          case _                     => Int
        }
        compileAndConvert(lhs, desiredType)
        compileAndConvert(rhs, desiredType)
        val codes = desiredType.codes
        expression match {
          case _: LogicAnd => codes.and(ch)
          case _: LogicOr  => codes.or(ch)
          case _: LogicXor => codes.xor(ch)
        }
      case shiftOp@ShiftOperatorTree(lhs, rhs)          =>
        val args = (lhs.getType, rhs.getType)
        val lhsDesired = args match {
          case _ if args.anyIs(Long) => Long
          case _                     => Int
        }
        compileAndConvert(lhs, lhsDesired)
        compileAndConvert(rhs, Int)

        val codes = lhsDesired.codes
        expression match {
          case _: LeftShift  => codes.leftShift(ch)
          case _: RightShift => codes.rightShift(ch)
        }
      case LocalIntIncrementDecrement(varSymbol, value) =>
        ch << IInc(localVariableMap(varSymbol), value)
      case Assign(to, expr)                             =>
        to match {
          case id: VariableID               =>
            val sym = id.getSymbol
            store(sym, () => compileAndConvert(expr, sym.getType), duplicate, () => ch << ArgLoad(0))
          case acc@Access(obj, application) =>
            // This is a field variable symbol
            val sym = application.asInstanceOf[VariableID].getSymbol
            store(sym, () => compileAndConvert(expr, sym.getType), duplicate, () => compileExpr(obj))
          case ArrayRead(arr, index)        =>
            compileExpr(arr)
            compileExpr(index)
            val arrayTpe = arr.getType.asInstanceOf[TArray].tpe
            compileAndConvert(expr, arrayTpe)
            val codes = arrayTpe.codes
            if (duplicate)
              codes.dup_x2(ch) // arrayref index value -> value arrayref index value
            codes.arrayStore(ch)
        }
      case Is(expr, tpe)                                =>
        compileExpr(expr)
        tpe.getType match {
          case o: TObject               => ch << InstanceOf(o.classSymbol.name)
          case primitive: PrimitiveType => ch << InstanceOf(primitive.koolWrapper)
        }
      case As(expr, tpe)                                =>
        if (expr.getType == tpe.getType) {
          compileExpr(expr)
        } else if (tpe.getType.isSubTypeOf(expr.getType)) {
          compileExpr(expr)
          val name = tpe.getType.asInstanceOf[TObject].classSymbol.name
          ch << CheckCast(name)
        } else {
          compileAndConvert(expr, tpe.getType)
        }
      case arrRead@ArrayRead(arr, index)                =>
        compileExpr(arr)
        compileExpr(index)
        val arrayTpe = arr.getType.asInstanceOf[TArray].tpe
        arrayTpe.codes.arrayLoad(ch)
      case acc@Access(obj, application)                 =>
        if (!acc.isStatic)
          compileExpr(obj)

        obj.getType match {
          case TArray(tpe) =>
            // If the object is an array this is the method call Size()
            // TODO: Find of way of not hardcoding this
            val mc = application.asInstanceOf[MethodCall]
            assert(mc.meth.name == "Size" && mc.args.isEmpty)
            ch << ARRAYLENGTH
            return
          case _           =>
        }

        val classSymbol = obj.getType.asInstanceOf[TObject].classSymbol
        val className = classSymbol.name

        application match {
          case id@Identifier(fieldName) =>
            val bytecode = id.getType.byteCodeName
            if (acc.isStatic)
              ch << GetStatic(className, fieldName, bytecode)
            else
              ch << GetField(className, fieldName, bytecode)
          case MethodCall(meth, args)   =>
            val methSymbol = meth.getSymbol
            val methName = meth.name
            val signature = methSymbol.byteCodeSignature

            // Static calls are executed with InvokeStatic.
            // Super calls and private calls are executed with invokespecial.
            // Methods called on traits are always called with invokeinterface
            // The rest are called with invokevirtual

            compileArguments(methSymbol, args)
            ch << (
              if (acc.isStatic)
                InvokeStatic(className, methName, signature)
              else if (obj.isInstanceOf[Super] || methSymbol.accessability == Private())
                InvokeSpecial(className, methName, signature)
              else if (classSymbol.isAbstract)
                InvokeInterface(className, methName, signature)
              else
                InvokeVirtual(className, methName, signature)
              )

            if (!duplicate && methSymbol.getType != TUnit)
              ch << POP
        }
      case Trees.New(tpe, args)                         =>
        tpe.getType match {
          case TObject(classSymbol) =>
            val argTypes = args.map(_.getType)
            ch << cafebabe.AbstractByteCodes.New(classSymbol.name)
            tpe.getType.codes.dup(ch)

            val signature = classSymbol.lookupMethod("new", argTypes) match {
              case Some(methodSymbol) =>
                compileArguments(methodSymbol, args)
                methodSymbol.byteCodeSignature
              case _                  => "()V"
            }
            // Constructors are always called with InvokeSpecial
            ch << InvokeSpecial(classSymbol.name, ConstructorName, signature)
          case primitiveType        =>
            // args size can only be 0 or 1
            if (args.size == 1)
              compileAndConvert(args.head, primitiveType)
            else
              primitiveType.codes.defaultConstant(ch)
        }
      case negOp@Negation(expr)                         =>
        compileExpr(expr)
        expr.getType.codes.negation(ch)
      case notOp@LogicNot(expr)                         =>
        compileExpr(expr)
        ch << Ldc(-1)
        expr.getType.codes.xor(ch)
      case hashOp@Hash(expr)                            =>
        expr.getType match {
          case x: TObject               =>
            compileExpr(expr)
            ch << InvokeVirtual(JavaObject, "hashCode", "()I")
          case primitive: PrimitiveType =>
            val className = primitive.javaWrapper
            ch << cafebabe.AbstractByteCodes.New(className) << DUP
            compileExpr(expr)
            ch << InvokeSpecial(className, ConstructorName, "(" + expr.getType.byteCodeName + ")V") <<
              InvokeVirtual(className, "hashCode", "()I")
        }
      case e@ExtractNullable(expr)                      =>
        compileAndConvert(expr, e.getType)
      case Ternary(condition, thn, els)                 =>
        val thnLabel = ch.getFreshLabel("then")
        val elsLabel = ch.getFreshLabel("else")
        val afterLabel = ch.getFreshLabel("after")
        val ternaryType = expression.getType
        compileBranch(condition, Label(thnLabel), Label(elsLabel))
        ch << Label(thnLabel)
        compileAndConvert(thn, ternaryType)
        ch << Goto(afterLabel)
        ch << Label(elsLabel)
        compileAndConvert(els, ternaryType)
        ch << Label(afterLabel)
      case PutValue(expr)                               =>
        compileExpr(expr)
      case GeneratedExpr(stats)                         =>
        // Generated expressions are always compiled, they cannot have useless expressions
        stats.foreach {
          case PutValue(expr) => if (duplicate)
            compileExpr(expr)
          case stat           => compileStat(stat, compileUseless = true)
        }
      case _                                            => ???
    }
  }

  private def compileArguments(methodSymbol: MethodSymbol, args: List[ExprTree]) =
    args.zip(methodSymbol.argList.map(_.getType)).foreach {
      case (givenExpr, expected) => compileAndConvert(givenExpr, expected)
    }

  private def compileAndConvert(expr: ExprTree, desired: Type): Unit = {
    val found = expr.getType
    if (found.isSubTypeOf(desired)) {
      compileExpr(expr)
      return
    }

    val codes = found.codes
    (found, desired) match {
      case (_, desired: TObject) if desired.implicitTypes.contains(found) =>
        val name = desired.classSymbol.name
        ch << cafebabe.AbstractByteCodes.New(name)
        desired.codes.dup(ch)
        compileExpr(expr)
        val signature = s"(${found.byteCodeName})V"
        ch << InvokeSpecial(name, ConstructorName, signature)
      case (found: TArray, desired: TArray)                               =>
        // Found an array and wanted an array, expr must be an arraylit
        // Convert each argument to the desired type
        expr match {
          case arrLit: ArrayLit => compileArrayLiteral(arrLit, Some(desired.tpe))
          case _                =>
            ???
        }
      case _                                                              =>
        compileExpr(expr)
        (found, desired) match {
          case (found: TObject, desired: PrimitiveType) if !desired.isNullable                           =>
            // Found an object, wanted a non nullable primitive type, unbox the object
            unbox(desired)
          case (found: PrimitiveType, desired: TObject)                                                  =>
            // found a primitive type, needed an object, box the primitive
            codes.box(ch)
          case (found: PrimitiveType, desired: PrimitiveType) if found.isNullable && !desired.isNullable =>
            // found nullable primitive, need non nullable primitive, unbox
            unbox(desired)
          case (found: PrimitiveType, desired: PrimitiveType) if !found.isNullable && desired.isNullable =>
            // found non nullable primitive, need nullable primitive, box
            codes.box(ch)
          case (found: PrimitiveType, desired: TDouble)                                                  =>
            codes.toDouble(ch)
          case (found: PrimitiveType, desired: TFloat)                                                   =>
            codes.toFloat(ch)
          case (found: PrimitiveType, desired: TLong)                                                    =>
            codes.toLong(ch)
          case _                                                                                         =>
            codes.toInt(ch)
        }
    }
  }

  private def unbox(to: PrimitiveType) = {
    val bcName = to.byteCodeName
    val className = to.koolWrapper
    ch << CheckCast(className) << InvokeVirtual(className, s"Value", s"()$bcName")
  }

  private def compileArrayLiteral(arrLit: ArrayLit, desiredType: Option[Type] = None) = {
    val expressions = arrLit.value
    val arrayType = arrLit.getType
    val newType = desiredType match {
      case Some(tpe) => tpe
      case None      => arrayType.asInstanceOf[TArray].tpe
    }

    ch << Ldc(arrLit.value.size)
    newType.codes.newArray(ch)

    expressions.zipWithIndex.foreach { case (expr, i) =>
      arrayType.codes.dup(ch)
      ch << Ldc(i)
      compileAndConvert(expr, newType)
      newType.codes.arrayStore(ch)
    }
  }

  private def compileBranch(expression: ExprTree, thn: Label, els: Label): Unit = expression match {
    case notOp@Not(expr)                         => compileBranch(expr, els, thn)
    case TrueLit()                               => ch << Goto(thn.id)
    case FalseLit()                              => ch << Goto(els.id)
    case And(lhs, rhs)                           =>
      val next = Label(ch.getFreshLabel("next"))
      compileBranch(lhs, next, els)
      ch << next
      compileBranch(rhs, thn, els)
    case Or(lhs, rhs)                            =>
      val next = Label(ch.getFreshLabel("next"))
      compileBranch(lhs, thn, next)
      ch << next
      compileBranch(rhs, thn, els)
    case compOp@ComparisonOperatorTree(lhs, rhs) =>
      val argTypes = (lhs.getType, rhs.getType)
      val desiredType = argTypes match {
        case _ if argTypes.anyIs(Double) => Double
        case _ if argTypes.anyIs(Float)  => Float
        case _ if argTypes.anyIs(Long)   => Long
        case _                           => Int
      }
      compileAndConvert(lhs, desiredType)
      compileAndConvert(rhs, desiredType)
      val codes = desiredType.codes
      expression match {
        case _: LessThan          => codes.cmpLt(ch, thn.id)
        case _: LessThanEquals    => codes.cmpLe(ch, thn.id)
        case _: GreaterThan       => codes.cmpGt(ch, thn.id)
        case _: GreaterThanEquals => codes.cmpGe(ch, thn.id)
        case _                    => ???
      }

      ch << Goto(els.id)
    case eqOp@EqualsOperatorTree(lhs, rhs)       =>
      def compileEquals(codes: CodeMap) = expression match {
        case _: Equals    => codes.cmpEq(ch, thn.id)
        case _: NotEquals => codes.cmpNe(ch, thn.id)
      }

      val argTypes = (lhs.getType, rhs.getType)
      argTypes match {
        case _ if argTypes.anyIs(TNull)  =>
          val toCompile = if (lhs.getType == TNull) rhs else lhs
          compileExpr(toCompile)
          expression match {
            case _: Equals    => ch << IfNull(thn.id)
            case _: NotEquals => ch << IfNonNull(thn.id)
          }
        case _ if argTypes.anyIs(Object) =>
          compileExpr(lhs)
          compileExpr(rhs)
          compileEquals(Object.codes)
        case _ if argTypes.anyIs(Array)  =>
          compileExpr(lhs)
          compileExpr(rhs)
          compileEquals(Array.codes)
        case _                           =>
          val desiredType = argTypes match {
            case _ if argTypes.anyIs(Object) => Object
            case _ if argTypes.anyIs(Array)  => Array
            case _ if argTypes.anyIs(Double) => Double
            case _ if argTypes.anyIs(Float)  => Float
            case _ if argTypes.anyIs(Long)   => Long
            case _                           => Int
          }
          compileAndConvert(lhs, desiredType)
          compileAndConvert(rhs, desiredType)
          compileEquals(desiredType.codes)
      }
      ch << Goto(els.id)
    case expr: ExprTree                          =>
      compileExpr(expr)
      expr match {
        case id: VariableID if id.getType.isNullable =>
          ch << IfNull(els.id)
        case _                                       =>
          ch << IfEq(els.id)
      }
      ch << Goto(thn.id) // If false go to else
    case _                                       => ???
  }

  private def store(variable: VariableSymbol,
    putValue: () => Unit,
    duplicate: Boolean,
    putObject: () => Unit = () => {
      ch << ArgLoad(0)
    }): CodeHandler = {
    val name = variable.name
    val tpe = variable.getType
    val codes = tpe.codes

    if (localVariableMap.contains(variable)) {
      val id = localVariableMap(variable)
      putValue()
      if (duplicate)
        codes.dup(ch)
      return tpe.codes.store(ch, id)
    }

    // variable is a field
    if (!variable.isStatic)
      putObject()

    // Must be a field since it's not a local variable
    val className = variable.asInstanceOf[FieldSymbol].classSymbol.name

    putValue()

    if (duplicate)
      if (variable.isStatic)
        codes.dup(ch)
      else
        codes.dup_x1(ch) // this value -> value this value

    if (variable.isStatic)
      ch << PutStatic(className, name, tpe.byteCodeName)
    else
      ch << PutField(className, name, tpe.byteCodeName)
  }

  private def load(variable: VariableSymbol): CodeHandler = {
    val name = variable.name
    val tpe = variable.getType

    if (localVariableMap.contains(variable)) {
      val id = localVariableMap(variable)
      return tpe.codes.load(ch, id)
    }

    // Must be a field since it's not a local variable
    val className = variable.asInstanceOf[FieldSymbol].classSymbol.name

    if (variable.isStatic) {
      ch << GetStatic(className, name, tpe.byteCodeName)
    } else {
      ch << ArgLoad(0) // this reference
      ch << GetField(className, name, tpe.byteCodeName)
    }
  }

  object LocalIntIncrementDecrement {

    // Unpacks expressions such as
    // a = a + 5
    // a = 5 + a
    // b = b - 5
    // where the operands are of type int
    def unapply(e: ExprTree): Option[(VariableSymbol, Int)] = e match {
      case Assign(to, expr) => to match {
        case v1: VariableID =>
          val (v2, value) = expr match {
            case Plus(v2: VariableID, IntLit(v))  => (v2, v)
            case Minus(v2: VariableID, IntLit(v)) => (v2, -v)
            case Plus(IntLit(v), v2: VariableID)  => (v2, v)
            case _                                => return None
          }
          val (s1, s2) = (v1.getSymbol, v2.getSymbol)
          if (value > 32767 || value < -32768)
            None
          else if (!localVariableMap.contains(s1) || !localVariableMap.contains(s2))
            None
          else if (localVariableMap(s1) != localVariableMap(s2))
            None
          else
            Some(v1.getSymbol, value)
        case _              => None
      }
      case _                => None
    }
  }

}