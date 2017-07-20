package tlang.compiler.code

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
import tlang.utils.Extensions._

import scala.collection.mutable

object CodeGenerator {

  val TraitFlags: U2 = CLASS_ACC_ABSTRACT | CLASS_ACC_PUBLIC | CLASS_ACC_INTERFACE
  val ClassFlags: U2 = CLASS_ACC_PUBLIC

  val ConstructorName = "<init>"

  val JavaLang = "java/lang/"
  val JavaIO   = "java/io/"

  /* Java classes used by T compiler */
  val JavaStringBuilder   : String = JavaLang + "StringBuilder"
  val JavaString          : String = JavaLang + "String"
  val JavaSystem          : String = JavaLang + "System"
  val JavaPrintStream     : String = JavaIO + "PrintStream"
  val JavaObject          : String = JavaLang + "Object"
  val JavaInt             : String = JavaLang + "Integer"
  val JavaChar            : String = JavaLang + "Character"
  val JavaFloat           : String = JavaLang + "Float"
  val JavaDouble          : String = JavaLang + "Double"
  val JavaLong            : String = JavaLang + "Long"
  val JavaBool            : String = JavaLang + "Boolean"
  val JavaRuntimeException: String = JavaLang + "RuntimeException"

  object Primitive {
    def unapply(tpe: Type): Option[TObject] = if (tpe in Primitives) Some(tpe.asInstanceOf[TObject]) else None
  }

  object NonPrimitive {
    def unapply(tpe: Type): Option[TObject] = tpe match {
      case t: TObject if !(t in Primitives) => Some(t)
      case _                                => None
    }
  }

  implicit class JVMClassSymbol(val s: ClassSymbol) extends AnyVal {
    def JVMName: String = s.name.replaceAll("::", "/")
  }

  implicit class JVMMethodSymbol(val s: MethodSymbol) extends AnyVal {

    def byteCodeSignature: String = {
      val types = s.argTypes.map(_.byteCodeName).mkString
      s"($types)${ s.getType.byteCodeName }"
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
      case TUnit                        => EmptyCodeMap
      case TNull                        => new ObjectCodeMap(JavaObject)
      case Primitive(p) if p.isNullable => new ObjectCodeMap(TRef)
      case Int                          => IntCodeMap
      case Long                         => LongCodeMap
      case Float                        => FloatCodeMap
      case Double                       => DoubleCodeMap
      case Char                         => CharCodeMap
      case Bool                         => BoolCodeMap
      case objTpe: TObject              => new ObjectCodeMap(objTpe.classSymbol.JVMName)
      case TArray(arrTpe)               => new ArrayCodeMap(arrTpe.byteCodeName)
    }

    def size: Int = t match {
      case TUnit         => 0
      case Long | Double => 2
      case _             => 1
    }
  }
}

class CodeGenerator(ch: CodeHandler, localVariableMap: mutable.Map[VariableSymbol, Int]) {

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
      case PrintExprTree(expr)                       =>
        ch << GetStatic(JavaSystem, "out", "L" + JavaPrintStream + ";")
        compileExpr(expr)
        val arg = expr.getType match {
          case NonPrimitive(_) => s"L$JavaObject;"
          case Primitive(p)    => if (p.isNullable) s"L$JavaObject;" else p.byteCodeName
          case _               => ???
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
      case branch: BranchingOperatorTree                =>
        compileValueBranch(branch)
      case arrLit: ArrayLit                             =>
        compileArrayLiteral(arrLit)
      case newArray@Trees.NewArray(tpe, sizes)          =>
        sizes foreach (compileExpr(_))
        val dimension = newArray.dimension
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
            store(sym, () => compileAndConvert(expr, sym.getType), duplicate, () => ch << ArgLoad(0))
          case Access(obj, application) =>
            // This is a field variable symbol
            val sym = application.asInstanceOf[VariableID].getSymbol
            store(sym, () => compileAndConvert(expr, sym.getType), duplicate, () => compileExpr(obj))
          case ArrayRead(arr, index)    =>
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
              val signature = meth.getSymbol.byteCodeSignature

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
            }

            if (!duplicate && methSymbol.getType != TUnit)
              ch << POP
        }
      case newTree@Trees.New(tpe, args)                 =>
        tpe.getType match {
          case Primitive(primitiveType) =>
            // args size can only be 0 or 1 for primitive types
            if (args.size == 1)
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

  private def compileArguments(methodSymbol: MethodSymbol, args: List[ExprTree]) =
    args.zip(methodSymbol.argTypes).foreach {
      case (givenExpr, expected) => compileAndConvert(givenExpr, expected)
    }

  private def compileAndConvert(expr: ExprTree, desired: Type): Unit = {
    val found = expr.getType
    val codes = found.codes
    (found, desired) match {
      case (NonPrimitive(found), NonPrimitive(desired)) if found.isSubTypeOf(desired) =>
        compileExpr(expr)
      case (_, NonPrimitive(desired)) if desired.implicitTypes.contains(found)        =>
        val name = desired.classSymbol.JVMName
        ch << cafebabe.AbstractByteCodes.New(name)
        desired.codes.dup(ch)
        compileExpr(expr)
        val signature = s"(${ found.byteCodeName })V"
        ch << InvokeSpecial(name, ConstructorName, signature)
      case (_: TArray, desired: TArray)                                               =>
        expr match {
          case arrLit: ArrayLit => compileArrayLiteral(arrLit, Some(desired.tpe))
          case _                => compileExpr(expr)
        }
      case _                                                                          =>
        compileExpr(expr)
        (found, desired) match {
          case (NonPrimitive(_), Primitive(desired)) if !desired.isNullable                      =>
            // Found an object, wanted a non nullable primitive type, unbox the object
            unbox(desired)
          case (Primitive(_), NonPrimitive(_))                                                   =>
            // found a primitive type, needed an object, box the primitive
            codes.box(ch)
          case (Primitive(found), Primitive(desired)) if found.isNullable && !desired.isNullable =>
            // found nullable primitive, need non nullable primitive, unbox
            unbox(desired)
          case (Primitive(found), Primitive(desired)) if !found.isNullable && desired.isNullable =>
            // found non nullable primitive, need nullable primitive, box
            codes.box(ch)
          case (_, Double)                                                                       =>
            codes.toDouble(ch)
          case (_, Float)                                                                        =>
            codes.toFloat(ch)
          case (_, Long)                                                                         =>
            codes.toLong(ch)
          case _                                                                                 =>
            codes.toInt(ch)
        }
    }
  }

  private def unbox(to: Type) = {
    val bcName = to.byteCodeName
    val className = to.TRef
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
    case Not(expr)                                                             => compileBranch(expr, els, thn)
    case TrueLit()                                                             => ch << Goto(thn.id)
    case FalseLit()                                                            => ch << Goto(els.id)
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

  private def store(variable: VariableSymbol,
    putValue: () => Unit,
    duplicate: Boolean,
    putObject: () => Unit = () => {
      ch << ArgLoad(0)
    }): CodeHandler = {
    val name = variable.name
    val tpe = variable.getType
    val codes = tpe.codes

    localVariableMap.get(variable) match {
      case Some(id) =>
        putValue()
        if (duplicate)
          codes.dup(ch)
        tpe.codes.store(ch, id)
      case None     =>
        // variable is a field
        if (!variable.isStatic)
          putObject()

        // Must be a field since it's not a local variable
        val className = variable.asInstanceOf[FieldSymbol].classSymbol.JVMName

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
