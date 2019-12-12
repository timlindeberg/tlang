package tlang
package compiler
package code

import cafebabe.AbstractByteCodes.{InvokeVirtual, _}
import cafebabe.ByteCodes._
import cafebabe.ClassFileTypes._
import cafebabe.CodeHandler
import cafebabe.Flags._
import tlang.compiler.analyzer.Symbols._
import tlang.compiler.analyzer.Types
import tlang.compiler.analyzer.Types._
import tlang.compiler.ast.Trees
import tlang.compiler.ast.Trees._
import tlang.utils.Logging

import scala.collection.mutable

object CodeGenerator {

  val TraitFlags: U2 = CLASS_ACC_ABSTRACT | CLASS_ACC_PUBLIC | CLASS_ACC_INTERFACE
  val AnnotationFlags: U2 = TraitFlags | CLASS_ACC_ANNOTATION
  val ClassFlags: U2 = CLASS_ACC_PUBLIC

  val ConstructorName = "<init>"

  val JavaLang = "java/lang/"
  val JavaUtil = "java/util/"
  val JavaIO = "java/io/"

  /* Java classes used by T compiler */
  val JavaStringBuilder: String = JavaLang + "StringBuilder"
  val JavaString: String = JavaLang + "String"
  val JavaSystem: String = JavaLang + "System"
  val JavaPrintStream: String = JavaIO + "PrintStream"
  val JavaObject: String = JavaLang + "Object"
  val JavaInt: String = JavaLang + "Integer"
  val JavaChar: String = JavaLang + "Character"
  val JavaFloat: String = JavaLang + "Float"
  val JavaDouble: String = JavaLang + "Double"
  val JavaLong: String = JavaLang + "Long"
  val JavaBool: String = JavaLang + "Boolean"
  val JavaArrays: String = JavaUtil + "Arrays"
  val JavaRuntimeException: String = JavaLang + "RuntimeException"

  object Nullable {
    def unapply(tpe: Type): Option[Type] = if (tpe.isNullable) Some(tpe) else None
  }

  object NonNullable {
    def unapply(tpe: Type): Option[Type] = if (!tpe.isNullable) Some(tpe) else None
  }

  object Primitive {
    def unapply(tpe: Type): Option[TObject] = if (tpe in Primitives) Some(tpe.asInstanceOf[TObject]) else None
  }

  object NonPrimitive {
    def unapply(tpe: Type): Option[TObject] = tpe match {
      case t: TObject if t notIn Primitives => Some(t)
      case _                                => None
    }
  }

  implicit class JVMClassSymbol(val classSymbol: ClassSymbol) extends AnyVal {
    def JVMName: String = classSymbol.name.replaceAll("::", "/")
  }

  implicit class JVMMethodSymbol(val methSym: MethodSymbol) extends AnyVal {

    def byteCodeSignature: String = {
      val types = methSym.argTypes.map(_.byteCodeName).mkString
      s"($types)${ methSym.getType.byteCodeName }"
    }
  }

  implicit class JVMType(val t: Type) extends AnyVal {

    def javaWrapper: String = t match {
      case Int    => JavaInt
      case Long   => JavaLong
      case Float  => JavaFloat
      case Double => JavaDouble
      case Char   => JavaChar
      case Bool   => JavaBool
      case _      => ???
    }

    def TRef: String = {
      val name = t match {
        case Int    => "Int"
        case Long   => "Long"
        case Float  => "Float"
        case Double => "Double"
        case Char   => "Char"
        case Bool   => "Bool"
        case _      => ???
      }
      s"T/lang/${ name }Ref"
    }

    def byteCodeName: String = t match {
      case TUnit                        => "V"
      case TNull                        => s"L$JavaObject;"
      case Primitive(p) if p.isNullable => s"L$TRef;"
      case Int                          => "I"
      case Long                         => "J"
      case Float                        => "F"
      case Double                       => "D"
      case Char                         => "C"
      case Bool                         => "Z"
      case objTpe: TObject              => "L" + objTpe.classSymbol.JVMName + ";"
      case TArray(arrTpe)               => "[" + arrTpe.byteCodeName
    }

    def codes: CodeMap = t match {
      case TUnit                  => EmptyCodeMap
      case TNull                  => new ObjectCodeMap(JavaObject)
      case Primitive(Nullable(p)) => new ObjectCodeMap(TRef)
      case Int                    => IntCodeMap
      case Long                   => LongCodeMap
      case Float                  => FloatCodeMap
      case Double                 => DoubleCodeMap
      case Char                   => CharCodeMap
      case Bool                   => BoolCodeMap
      case objTpe: TObject        => new ObjectCodeMap(objTpe.classSymbol.JVMName)
      case TArray(arrTpe)         => new ArrayCodeMap(arrTpe.byteCodeName)
    }

    def size: Int = t match {
      case TUnit         => 0
      case Nullable(_)   => 1
      case Long | Double => 2
      case _             => 1
    }
  }
}

class CodeGenerator(ch: CodeHandler, localVariableMap: mutable.Map[VariableSymbol, Int]) extends Logging {

  import CodeGenerator._

  def compileStat(statement: StatTree,
    continue: Option[String] = None,
    break: Option[String] = None): Unit = {

    ch << LineNumber(statement.line)
    statement match {
      case UselessStatement(expr)                    =>
        compileExpr(expr)
        expr.getType match {
          case Types.TUnit =>
          case tpe         => tpe.codes.pop(ch)
        }
      case Block(stats)                              =>
        stats.foreach(compileStat(_, continue, break))
      case v@VarDecl(_, _, init, _, _)               =>
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
        compileStat(thnStat, continue, break)
        if (elsStat.isDefined)
          ch << Goto(after)
        ch << Label(els)
        if (elsStat.isDefined)
          compileStat(elsStat.get, continue, break)
        ch << Label(after)
      case While(conditionExpr, stat)                =>
        val body = ch.getFreshLabel("body")
        val condition = ch.getFreshLabel("condition")
        val after = ch.getFreshLabel("after")

        ch << Goto(condition)
        ch << Label(body)
        compileStat(stat, Some(condition), Some(after))
        ch << Label(condition)
        compileBranch(conditionExpr, Label(body), Label(after))
        ch << Label(after)
      case For(init, conditionExpr, postExprs, stat) =>
        val body = ch.getFreshLabel("body")
        val condition = ch.getFreshLabel("condition")
        val after = ch.getFreshLabel("after")
        val post = ch.getFreshLabel("post")

        init.foreach(stat => compileStat(stat))
        ch << Goto(condition)
        ch << Label(body)
        compileStat(stat, Some(post), Some(after))
        ch << Label(post)
        postExprs.foreach(expr => compileStat(expr))
        ch << Label(condition)
        compileBranch(conditionExpr, Label(body), Label(after))
        ch << Label(after)
      case PrintStatTree(expr)                       =>
        ch << GetStatic(JavaSystem, "out", s"L$JavaPrintStream;")
        compileExpr(expr)

        val arg = expr.getType match {
          case NonPrimitive(_) => s"L$JavaObject;"
          case Primitive(p)    => if (p.isNullable) s"L$JavaObject;" else p.byteCodeName
          case arr@TArray(t)   =>
            // Use deepToString to render multidimensional arrays
            val method = if (arr.dimension > 1) "deepToString" else "toString"
            val arrTpe = t match {
              case NonPrimitive(_) | TArray(_) => s"L$JavaObject;"
              case Primitive(p)                => if (p.isNullable) s"L$JavaObject;" else p.byteCodeName
            }

            ch << InvokeStatic(JavaArrays, method, s"([$arrTpe)L$JavaString;")
            s"L$JavaObject;"
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
        val stringName = Types.String.classSymbol.JVMName
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
    ch << LineNumber(expression.line)
    expression match {
      case TrueLit()                                    => ch << Ldc(1)
      case FalseLit()                                   => ch << Ldc(0)
      case NullLit()                                    => ch << ACONST_NULL
      case IntLit(value)                                => ch << Ldc(value)
      case LongLit(value)                               => ch << Ldc(value)
      case CharLit(value)                               => ch << Ldc(value)
      case FloatLit(value)                              => ch << Ldc(value)
      case DoubleLit(value)                             => ch << Ldc(value)
      case StringLit(value)                             => ch << Ldc(value)
      case id: VariableID                               => load(id.getSymbol)
      case _: This                                      => ch << ArgLoad(0)
      case _: Super                                     => ch << ArgLoad(0)
      case branch: BranchingOperatorTree                => compileValueBranch(branch)
      case arrLit: ArrayLit                             => compileArrayLiteral(arrLit)
      case newArray@Trees.NewArray(tpe, sizes)          =>
        sizes foreach (compileExpr(_))
        val dimension = newArray.sizes.size
        val arrType = tpe.getType.asInstanceOf[TArray].tpe
        if (dimension == 1)
          arrType.codes.newArray(ch)
        else
          ch << NewMultidimensionalArray(tpe.getType.byteCodeName, dimension)
      case LocalIntIncrementDecrement(varSymbol, value) =>
        ch << IInc(localVariableMap(varSymbol), value)
      case Assign(to, expr)                             =>
        to match {
          case id: VariableID           =>
            val sym = id.getSymbol
            store(expr, sym, duplicate, () => ch << ArgLoad(0))
          case Access(obj, application) =>
            // This is a field variable symbol
            val sym = application.asInstanceOf[VariableID].getSymbol
            store(expr, sym, duplicate, () => compileExpr(obj))
          case ArrayRead(arr, index)    =>
            compileExpr(arr)
            compileExpr(index)
            val arrayTpe = arr.getType.asInstanceOf[TArray].tpe
            expr match {
              case arrLit: ArrayLit =>
                val expectedInnerType = to.getType.asInstanceOf[TArray].tpe
                compileArrayLiteral(arrLit, Some(expectedInnerType))
              case _                => compileExpr(expr)
            }
            if (duplicate)
              expr.getType.codes.dup_x2(ch)
            convertValueOnStack(expr.getType, arrayTpe)
            arrayTpe.codes.arrayStore(ch)
        }
      case Is(expr, tpe)                                =>
        compileExpr(expr)

        val jvmTypeName = tpe.getType match {
          case arrTpe: TArray => arrTpe.byteCodeName
          case obj: TObject   => obj match {
            case NonPrimitive(_)      => obj.classSymbol.JVMName
            case Primitive(primitive) => primitive.TRef
          }
          case _              => ???
        }
        ch << InstanceOf(jvmTypeName)
      case As(expr, tpe)                                =>
        (expr.getType, tpe.getType) match {
          case (NonPrimitive(_), NonPrimitive(tpe)) =>
            compileExpr(expr)
            val name = tpe.classSymbol.JVMName
            ch << CheckCast(name)
          case _                                    =>
            compileAndConvert(expr, tpe.getType)
        }
      case ArrayRead(arr, index)                        =>
        compileExpr(arr)
        compileExpr(index)
        val arrayTpe = arr.getType.asInstanceOf[TArray].tpe
        arrayTpe.codes.arrayLoad(ch)
      case acc@Access(obj, application)                 =>
        if (!acc.isStatic)
          compileExpr(obj)

        obj.getType match {
          case _: TArray =>
            // If the object is an array this is the method call Size()
            // TODO: Find of way of not hardcoding this
            val mc = application.asInstanceOf[MethodCall]
            assert(mc.meth.name == "Size" && mc.args.isEmpty)
            ch << ARRAYLENGTH
            return
          case _         =>
        }

        val classSymbol = obj.getType.asInstanceOf[TObject].classSymbol
        val className = classSymbol.JVMName

        application match {
          case id@Identifier(fieldName) =>
            val bytecode = id.getType.byteCodeName
            if (acc.isStatic)
              ch << GetStatic(className, fieldName, bytecode)
            else
              ch << GetField(className, fieldName, bytecode)
          case MethodCall(meth, args)   =>
            val methSymbol = meth.getSymbol

            if (isPrimitiveOperatorCall(acc)) {
              compilePrimitiveOperatorCall(acc)
            } else {
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
                else if (obj.isInstanceOf[Super] || methSymbol.accessibility == Private())
                  InvokeSpecial(className, methName, signature)
                else if (classSymbol.isAbstract)
                  InvokeInterface(className, methName, signature)
                else
                  InvokeVirtual(className, methName, signature)
                )
            }

            val tpe = methSymbol.getType
            if (!duplicate)
              tpe.codes.pop(ch)
        }
      case newTree@Trees.New(tpe, args)                 =>
        tpe.getType match {
          case Primitive(primitiveType) =>
            // args size can only be 0 or 1 for primitive types
            if (args.lengthCompare(1) == 0)
              compileAndConvert(args.head, primitiveType)
            else
              primitiveType.codes.defaultConstant(ch)
          case TObject(classSymbol)     =>
            ch << cafebabe.AbstractByteCodes.New(classSymbol.JVMName)
            tpe.getType.codes.dup(ch)
            val methodSymbol = newTree.getSymbol
            compileArguments(methodSymbol, args)
            // Constructors are always called with InvokeSpecial
            ch << InvokeSpecial(classSymbol.JVMName, ConstructorName, methodSymbol.byteCodeSignature)
        }
      case ExtractNullable(expr)                        =>
        compileAndConvert(expr, expression.getType)
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
      case PutOnStack(expr)                             =>
        compileExpr(expr)
      case GeneratedExpr(stats)                         =>
        stats.foreach {
          case PutOnStack(expr) =>
            if (duplicate)
              compileExpr(expr)
          case stat             => compileStat(stat)
        }
    }
  }

  def compileField(classDecl: ClassDeclTree, varDecl: VarDecl): CodeHandler = {
    val VarDecl(id, _, Some(init), _, _) = varDecl
    val sym = id.getSymbol
    if (!sym.isStatic)
      ch << ArgLoad(0) // put this-reference on stack

    compileAndConvert(init, id.getType)
    val className = classDecl.getSymbol.JVMName
    val fieldName = id.getSymbol.name
    val typeName = sym.getType.byteCodeName

    if (sym.isStatic)
      ch << PutStatic(className, fieldName, typeName)
    else
      ch << PutField(className, fieldName, typeName)
  }

  private def isPrimitiveOperatorCall(acc: Access): Boolean = {
    acc.isStatic && Primitive.unapply(acc.obj.getType).isDefined
  }

  private def compilePrimitiveOperatorCall(acc: Access) = acc match {
    case Access(_, MethodCall(meth, args)) =>
      val methName = meth.name.drop(1)
      args.length match {
        case 1 =>
          val expr = args.head
          compilePrimitiveUnaryOperatorCall(expr, methName)
        case 2 => // binary
          val lhs = args(0)
          val rhs = args(1)
          compilePrimitiveBinaryOperatorCall(acc, lhs, rhs, methName)
      }
  }

  private def compilePrimitiveUnaryOperatorCall(expr: ExprTree, methName: String) = {
    val tpe = expr.getType
    val codes = tpe.codes
    methName match {
      case "Negation" =>
        compileExpr(expr)
        codes.negation(ch)
      case "Hash"     =>
        val className = tpe.javaWrapper
        ch << cafebabe.AbstractByteCodes.New(className) << DUP
        compileExpr(expr)
        ch << InvokeSpecial(className, ConstructorName, "(" + expr.getType.byteCodeName + ")V") <<
          InvokeVirtual(className, "hashCode", "()I")
      case "LogicNot" =>
        compileExpr(expr)
        ch << Ldc(-1)
        codes.xor(ch)
    }
  }

  private def compilePrimitiveBinaryOperatorCall(acc: Access, lhs: ExprTree, rhs: ExprTree, methName: String) = {
    methName match {
      case "LessThan" | "LessThanEquals" | "GreaterThan" | "GreaterThanEquals" | "Equals" | "NotEquals" =>
        compileValueBranch(acc)
      case _                                                                                            =>
        val args = (lhs.getType, rhs.getType)
        val desiredType = args match {
          case _ if anyIs(args, Double) => Double
          case _ if anyIs(args, Float)  => Float
          case _ if anyIs(args, Long)   => Long
          case _                        => Int
        }

        // Right hand side of shift operator is always converted to Int
        val rhsType = if (methName in List("LeftShift", "RightShift")) Int else desiredType
        compileAndConvert(lhs, desiredType)
        compileAndConvert(rhs, rhsType)

        val codes = desiredType.codes

        methName match {
          case "Plus"       => codes.add(ch)
          case "Minus"      => codes.sub(ch)
          case "Times"      => codes.mul(ch)
          case "Div"        => codes.div(ch)
          case "Modulo"     => codes.mod(ch)
          case "LogicAnd"   => codes.and(ch)
          case "LogicOr"    => codes.or(ch)
          case "LogicXor"   => codes.xor(ch)
          case "LeftShift"  => codes.leftShift(ch)
          case "RightShift" => codes.rightShift(ch)
          case _            =>
        }
    }
  }

  private def compilePrimitiveBranchingOperatorCall(lhs: ExprTree, rhs: ExprTree, methName: String, label: Label) = {
    val args = (lhs.getType, rhs.getType)
    val desiredType = args match {
      case _ if anyIs(args, Object) => Object
      case _ if anyIs(args, Array)  => Array
      case _ if anyIs(args, Double) => Double
      case _ if anyIs(args, Float)  => Float
      case _ if anyIs(args, Long)   => Long
      case _                        => Int
    }

    compileAndConvert(lhs, desiredType)
    compileAndConvert(rhs, desiredType)

    val codes = desiredType.codes

    methName match {
      case "LessThan"          => codes.cmpLt(ch, label.id)
      case "LessThanEquals"    => codes.cmpLe(ch, label.id)
      case "GreaterThan"       => codes.cmpGt(ch, label.id)
      case "GreaterThanEquals" => codes.cmpGe(ch, label.id)
      case "Equals"            => codes.cmpEq(ch, label.id)
      case "NotEquals"         => codes.cmpNe(ch, label.id)
      case _                   => ???
    }
  }

  private def compileValueBranch(condition: ExprTree) = {
    val thn = ch.getFreshLabel("then")
    val els = ch.getFreshLabel("else")
    val after = ch.getFreshLabel("after")
    compileBranch(condition, Label(thn), Label(els))
    ch << Label(thn)
    ch << Ldc(1)
    ch << Goto(after)
    ch << Label(els)
    ch << Ldc(0)
    ch << Label(after)
  }

  private def compileArguments(methodSymbol: MethodSymbol, args: List[ExprTree]): Unit =
    args.zip(methodSymbol.argTypes).foreach {
      case (givenExpr, expected) => compileAndConvert(givenExpr, expected)
    }

  private def compileAndConvert(expr: ExprTree, desired: Type): Unit = {
    (expr, desired) match {
      case (arrLit: ArrayLit, desired: TArray) => compileArrayLiteral(arrLit, Some(desired.tpe))
      case _                                   => compileExpr(expr)
    }
    convertValueOnStack(expr.getType, desired)
  }

  private def convertValueOnStack(found: Type, desired: Type): Unit = {
    val codes = found.codes
    (found, desired) match {
      case (NonPrimitive(found), NonPrimitive(desired)) if found.isSubTypeOf(desired) =>
      case (_, NonPrimitive(desired)) if desired.implicitTypes.contains(found)        =>

        // A hack: We already have a value on the stack that we want to convert
        // but to do an implicit construction we need to put the newly constructed
        // object before the value on the stack. We store it in memory temporarily
        // so we can put two new objects on the stack before, one to be used
        // by the constructor and one to replace the value that was on the stack.
        // This should probably be fixed at some point
        val id = ch.getFreshVar(found.size)
        codes.store(ch, id)

        val name = desired.classSymbol.JVMName
        ch << cafebabe.AbstractByteCodes.New(name)
        desired.codes.dup(ch)
        codes.load(ch, id)
        val signature = s"(${ found.byteCodeName })V"
        ch << InvokeSpecial(name, ConstructorName, signature)
      case (_: TArray, _: TArray)                                                     =>
      case (NonPrimitive(_), Primitive(NonNullable(desired)))                         =>
        // Found an object, wanted a non nullable primitive type, unbox the object
        unbox(desired)
      case (Primitive(_), NonPrimitive(_))                                            =>
        // found a primitive type, needed an object, box the primitive
        codes.box(ch)
      case (Primitive(Nullable(found)), Primitive(NonNullable(desired)))              =>
        // found nullable primitive, need non nullable primitive, unbox
        unbox(found)
        // The unboxed type might still be different from the desired type
        // for instance if twe have Found: Float? and desired Double
        castPrimitive(found, desired)
      case (Primitive(NonNullable(found)), Primitive(Nullable(desired)))              =>
        // found non nullable primitive, need nullable primitive, cast if necessary
        // and then box the value
        castPrimitive(found, desired)
        desired.getNonNullable.codes.box(ch)
      case _                                                                          =>
        // Otherwise we have to non nullable primitives. Cast if necessary
        castPrimitive(found, desired)
    }
  }

  private def castPrimitive(found: Type, desired: Type): Unit = {
    val codes = found.getNonNullable.codes
    desired match {
      case Double => codes.toDouble(ch)
      case Float  => codes.toFloat(ch)
      case Long   => codes.toLong(ch)
      case _      => codes.toInt(ch)
    }
  }

  private def unbox(to: Type) = {
    val bcName = to.getNonNullable.byteCodeName
    val className = to.TRef
    ch << CheckCast(className) << InvokeVirtual(className, s"Value", s"()$bcName")
  }

  private def compileArrayLiteral(arrLit: ArrayLit, desiredType: Option[Type] = None): Unit = {
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
    case Not(expr)                                                             => compileBranch(expr, els, thn)
    case And(lhs, rhs)                                                         =>
      val next = Label(ch.getFreshLabel("next"))
      compileBranch(lhs, next, els)
      ch << next
      compileBranch(rhs, thn, els)
    case Or(lhs, rhs)                                                          =>
      val next = Label(ch.getFreshLabel("next"))
      compileBranch(lhs, thn, next)
      ch << next
      compileBranch(rhs, thn, els)
    case EqualsOperatorTree(lhs, rhs)                                          =>
      val t1 = lhs.getType
      val t2 = rhs.getType
      if (t1 == TNull || t2 == TNull) {
        val toCompile = if (t1 == TNull) rhs else lhs
        compileExpr(toCompile)
        expression match {
          case _: Equals    => ch << IfNull(thn.id)
          case _: NotEquals => ch << IfNonNull(thn.id)
        }
      } else if (t1.isInstanceOf[TArray] || t2.isInstanceOf[TArray]) {
        compileExpr(lhs)
        compileExpr(rhs)
        expression match {
          case _: Equals    => Array.codes.cmpEq(ch, thn.id)
          case _: NotEquals => Array.codes.cmpNe(ch, thn.id)
        }
      }
      ch << Goto(els.id)
    case acc@Access(_, MethodCall(meth, args)) if isPrimitiveOperatorCall(acc) =>
      val methName = meth.name.drop(1)
      val lhs = args(0)
      val rhs = args(1)
      compilePrimitiveBranchingOperatorCall(lhs, rhs, methName, thn)
      ch << Goto(els.id)
    case expr: ExprTree                                                        =>

      compileExpr(expr)
      if (expr.getType.isNullable)
        ch << IfNull(els.id)
      else
        ch << IfEq(els.id)

      ch << Goto(thn.id) // If false go to else
    case _                                                                     => ???
  }

  private def store(expr: ExprTree, variable: VariableSymbol, duplicate: Boolean, putObject: () => Unit): CodeHandler = {
    val name = variable.name
    val tpe = variable.getType

    localVariableMap.get(variable) match {
      case Some(id) =>
        compileExpr(expr)
        if (duplicate)
          expr.getType.codes.dup(ch)
        convertValueOnStack(expr.getType, variable.getType)
        tpe.codes.store(ch, id)
      case None     =>
        // variable is a field
        if (!variable.isStatic)
          putObject()

        // Must be a field since it's not a local variable
        val className = variable.asInstanceOf[FieldSymbol].classSymbol.JVMName

        compileExpr(expr)
        if (duplicate)
          if (variable.isStatic)
            expr.getType.codes.dup(ch)
          else
            expr.getType.codes.dup_x1(ch) // this value -> value this value
        convertValueOnStack(expr.getType, variable.getType)
        if (variable.isStatic)
          ch << PutStatic(className, name, tpe.byteCodeName)
        else
          ch << PutField(className, name, tpe.byteCodeName)
    }
  }

  private def load(variable: VariableSymbol): CodeHandler = {
    val name = variable.name
    val tpe = variable.getType

    localVariableMap.get(variable) match {
      case Some(id) => tpe.codes.load(ch, id)
      case None     =>
        // Must be a field since it's not a local variable
        val className = variable.asInstanceOf[FieldSymbol].classSymbol.JVMName

        if (variable.isStatic) {
          ch << GetStatic(className, name, tpe.byteCodeName)
        } else {
          ch << ArgLoad(0) // this reference
          ch << GetField(className, name, tpe.byteCodeName)
        }
    }
  }

  private def anyIs(tpes: (Type, Type), tpe: Type) = tpes._1 == tpe || tpes._2 == tpe

  object LocalIntIncrementDecrement {

    // Unpacks expressions such as
    // a = a + 5
    // a = 5 + a
    // b = b - 5
    // where the operands are of type int
    // Such expressions can be optimized to use the IINC bytecode
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
