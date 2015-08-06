package tcompiler
package code

import java.io.File

import cafebabe.AbstractByteCodes._
import cafebabe.ByteCodes._
import cafebabe._
import tcompiler.analyzer.Symbols._
import tcompiler.analyzer.Types._
import tcompiler.analyzer._
import tcompiler.ast.TreeGroups._
import tcompiler.ast.Trees._
import tcompiler.utils._

import scala.collection.mutable

object CodeGeneration extends Pipeline[Program, Unit] {

  def run(ctx: Context)(prog: Program): Unit = {

    val outDir = ctx.outDir.map(_.getPath + "/").getOrElse("")
    val sourceName = ctx.file.getName

    // output code
    if (prog.main.isDefined)
      generateMainClassFile(sourceName, prog.main.get, outDir)

    prog.classes.foreach {
      case c: InternalClassDecl => generateClassFile(sourceName, c, outDir)
      case _                    =>
    }
  }

  /** Writes the proper .class file in a given directory. An empty string for dir is equivalent to "./". */
  def generateClassFile(sourceName: String, classDecl: ClassDecl, dir: String): Unit = {
    val sym = classDecl.getSymbol
    val classFile = new ClassFile(sym.name, sym.parent.map(_.name))

    classFile.setSourceFile(classDecl.file.getName)

    classDecl.vars.foreach { varDecl =>
      val varSymbol = varDecl.getSymbol
      classFile.addField(varSymbol.getType.byteCodeName, varSymbol.name).setFlags(getFlags(varDecl))
    }


    var hasConstructor = false
    classDecl.methods.foreach { methodDecl =>
      val methSymbol = methodDecl.getSymbol

      val methodHandle = methodDecl match {
        case mt: MethodDecl       =>
          val argTypes = methSymbol.argList.map(_.getType.byteCodeName).mkString
          classFile.addMethod(methSymbol.getType.byteCodeName, methSymbol.name, argTypes)
        case con: ConstructorDecl =>
          hasConstructor = true
          generateConstructor(Some(con), classFile, classDecl)
        case op: OperatorDecl     =>
          val argTypes = methSymbol.argList.map(_.getType.byteCodeName).mkString
          val operatorSymbol = op.getSymbol.asInstanceOf[OperatorSymbol]
          classFile.addMethod(methSymbol.getType.byteCodeName, operatorSymbol.methodName, argTypes)
      }
      methodHandle.setFlags(getFlags(methodDecl))
      generateMethod(methodHandle.codeHandler, methodDecl)
    }

    if (!hasConstructor)
      generateDefaultConstructor(classFile, classDecl)

    val file = getFilePath(dir, sym)
    classFile.writeToFile(file)
  }

  def generateMethod(ch: CodeHandler, mt: FuncTree): Unit = {
    val methSym = mt.getSymbol
    val variableMap = mutable.HashMap[String, Int]()

    var offset = if (mt.isStatic) 0 else 1

    mt.args.zipWithIndex.foreach {
      case (arg, i) =>
        variableMap(arg.getSymbol.name) = i + offset
        if (arg.getSymbol.getType.size == 2) {
          // Longs and doubles take up two slots
          offset += 1
        }
    }
    val codeGenerator = new CodeGenerator(ch, methSym.classSymbol.name, variableMap)
    initializeLocalVariables(mt, codeGenerator, variableMap)

    mt.stats.foreach(codeGenerator.compileStat)

    addReturnStatement(ch, mt)
    ch.freeze
  }

  def generateDefaultConstructor(classFile: ClassFile, classDecl: ClassDecl) = {
    val mh = generateConstructor(None, classFile, classDecl)
    val ch = mh.codeHandler
    ch << RETURN
    ch.freeze
  }

  def generateConstructor(con: Option[ConstructorDecl], classFile: ClassFile, classDecl: ClassDecl): MethodHandler = {
    val mh = con match {
      case Some(conDecl) =>
        val argTypes = conDecl.getSymbol.argList.map(_.getType.byteCodeName).mkString
        classFile.addConstructor(argTypes)
      case _ =>
        classFile.addConstructor(Nil)
    }

    val ch = mh.codeHandler

    // Initialize fields after constructor
    val codeGenerator = new CodeGenerator(ch, classDecl.getSymbol.name, mutable.HashMap())
    classDecl.vars.foreach {
      case VarDecl(varTpe, id, Some(expr), _) =>
        ch << ArgLoad(0) // put this-reference on stack
        codeGenerator.compileExpr(expr)
        ch << PutField(classDecl.getSymbol.name, id.value, varTpe.getType.byteCodeName)
      case _                                  =>
    }

    addSuperCall(mh, classDecl)
    mh
  }

  def generateMainClassFile(sourceName: String, main: MainObject, dir: String) = {
    val mainClassFile = new ClassFile(main.id.value, None)
    mainClassFile.setSourceFile(sourceName)
    generateMainMethod(mainClassFile.addMainMethod.codeHandler, main.stats, main.id.value)
    mainClassFile.addDefaultConstructor
    val file = getFilePath(dir, main.getSymbol)
    mainClassFile.writeToFile(file)
  }

  def generateMainMethod(ch: CodeHandler, stmts: List[StatTree], cname: String): Unit = {
    stmts.foreach(new CodeGenerator(ch, cname, mutable.HashMap()).compileStat)
    ch << RETURN
    ch.freeze
  }

  private def getFlags(obj: Modifiable) = {
    var flags: Int = 0
    val isMethod = obj match {
      case _: MethodDecl | _: ConstructorDecl | _: OperatorDecl => true
      case _                                                    => false
    }

    obj.modifiers.foreach {
      case Public    => flags |= (if (isMethod) Flags.METHOD_ACC_PUBLIC else Flags.FIELD_ACC_PUBLIC)
      case Private   => flags |= (if (isMethod) Flags.METHOD_ACC_PRIVATE else Flags.FIELD_ACC_PRIVATE)
      case Protected => flags |= (if (isMethod) Flags.METHOD_ACC_PROTECTED else Flags.FIELD_ACC_PROTECTED)
      case Static    => flags |= (if (isMethod) Flags.METHOD_ACC_STATIC else Flags.FIELD_ACC_STATIC)
    }
    flags.asInstanceOf[Short]
  }

  private def getFilePath(outDir: String, sym: ClassSymbol): String = {
    val split = sym.name.split("/")
    val packageDir = split.take(split.size - 1).mkString("/")
    val filePath = outDir + packageDir
    val f = new File(filePath)
    if (!f.getAbsoluteFile.exists()) {
      if (!f.mkdirs())
        sys.error(s"Could not create output directory '${f.getAbsolutePath}'.")
    }
    outDir + sym.name + ".class"
  }

  private def addReturnValueAndStatement(ch: CodeHandler, tpe: Type) = tpe match {
    case TUnit => ch << RETURN
    case _     =>
      tpe.codes.defaultConstant(ch)
      tpe.codes.ret(ch)
  }

  private def addReturnStatement(ch: CodeHandler, mt: FuncTree) = ch.peek match {
    case Some(byteCode) => byteCode match {
      case ARETURN | IRETURN | RETURN | DRETURN | FRETURN | LRETURN =>
      case _                                                        =>
        mt match {
          case mt: MethodDecl     => addReturnValueAndStatement(ch, mt.retType.getType)
          case op: OperatorDecl   => addReturnValueAndStatement(ch, op.retType.getType)
          case _: ConstructorDecl => ch << RETURN
        }
    }
    case None           => ch << RETURN
  }

  private def addSuperCall(mh: MethodHandler, ct: ClassDecl) = {
    val superClassName = ct.parent match {
      case Some(name) => name.value
      case None       => CodeGenerator.JavaObject
    }

    mh.codeHandler << ALOAD_0
    mh.codeHandler << InvokeSpecial(superClassName, CodeGenerator.ConstructorName, "()V")
  }

  private def initializeLocalVariables(mt: FuncTree, codeGenerator: CodeGenerator, variableMap: mutable.HashMap[String, Int]) = {
    val ch = codeGenerator.ch

    mt.vars foreach { case variable @ VarDecl(tpe, _, init, _) =>
      val id = ch.getFreshVar(tpe.getType.size)
      val name = variable.getSymbol.name
      val codes = tpe.getType.codes
      variableMap(name) = id
      init match {
        case Some(expr) =>
          codeGenerator.compileExpr(expr)
          codes.store(ch, variableMap(name))
        case None       =>
          codes.defaultConstant(ch)
          codes.store(ch, id)
      }
    }
  }
}

class CodeGenerator(val ch: CodeHandler, className: String, variableMap: mutable.HashMap[String, Int]) {

  def compileStat(statement: StatTree): Unit = {
    ch << LineNumber(statement.line)
    statement match {
      case Block(stats)                     => stats.foreach(compileStat)
      case If(expr, thn, els)               =>
        val thnLabel = ch.getFreshLabel(CodeGenerator.Then)
        val elsLabel = ch.getFreshLabel(CodeGenerator.Else)
        val afterLabel = ch.getFreshLabel(CodeGenerator.After)

        branch(expr, Label(thnLabel), Label(elsLabel))
        ch << Label(thnLabel)
        compileStat(thn)
        ch << Goto(afterLabel)
        ch << Label(elsLabel)
        if (els.isDefined) compileStat(els.get)
        ch << Label(afterLabel)
      case While(expr, stat)                =>
        val bodyLabel = ch.getFreshLabel(CodeGenerator.Body)
        val afterLabel = ch.getFreshLabel(CodeGenerator.After)
        branch(expr, Label(bodyLabel), Label(afterLabel))
        ch << Label(bodyLabel)
        compileStat(stat)
        branch(expr, Label(bodyLabel), Label(afterLabel))
        ch << Label(afterLabel)
      case For(init, condition, post, stat) =>
        val bodyLabel = ch.getFreshLabel(CodeGenerator.Body)
        val afterLabel = ch.getFreshLabel(CodeGenerator.After)
        init.foreach(compileStat)
        branch(condition, Label(bodyLabel), Label(afterLabel))
        ch << Label(bodyLabel)
        compileStat(stat)
        post.foreach(compileStat)
        branch(condition, Label(bodyLabel), Label(afterLabel))
        ch << Label(afterLabel)
      case PrintStatement(expr)             =>
        ch << GetStatic(CodeGenerator.JavaSystem, "out", "L" + CodeGenerator.JavaPrintStream + ";")
        compileExpr(expr)
        val arg = expr.getType match {
          case _: TObject => "L" + CodeGenerator.JavaObject + ";" // Call System.out.println(Object) for all other types
          case _          => expr.getType.byteCodeName
        }
        val funcName = statement match {
          case _: Print   => "print"
          case _: Println => "println"
        }
        ch << InvokeVirtual(CodeGenerator.JavaPrintStream, funcName, "(" + arg + ")V")
      case Return(Some(expr))               =>
        compileExpr(expr)
        expr.getType.codes.ret(ch)
      case Return(None)                     =>
        ch << RETURN
      case expr: ExprTree                   =>
        // Assignment, method call or increment/decrement
        compileExpr(expr, duplicate = false)
    }
  }

  def compileOperatorCall(ch: CodeHandler, expr: ExprTree, args: (Type, Type)): Boolean = {
    val argsList = List(args._1, args._2)
    compileOperatorCall(ch, args._1, expr, argsList) || compileOperatorCall(ch, args._2, expr, argsList)
  }

  def compileOperatorCall(ch: CodeHandler, expr: ExprTree, arg: Type): Boolean =
    compileOperatorCall(ch, arg, expr, List(arg))

  def compileOperatorCall(ch: CodeHandler, classType: Type, expr: ExprTree, args: List[Type]): Boolean =
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

  def compileExpr(expression: ExprTree, duplicate: Boolean = true): Unit = {
    ch << LineNumber(expression.line)
    expression match {
      case True()                           => ch << Ldc(1)
      case False()                          => ch << Ldc(0)
      case IntLit(value)                    => ch << Ldc(value)
      case LongLit(value)                   => ch << Ldc(value)
      case CharLit(value)                   => ch << Ldc(value)
      case FloatLit(value)                  => ch << Ldc(value)
      case DoubleLit(value)                 => ch << Ldc(value)
      case StringLit(value)                 => ch << Ldc(value)
      case id @ Identifier(value)           => load(id)
      case This()                           => ch << ArgLoad(0)
      case _: And |
           _: Or |
           _: Equals |
           _: NotEquals |
           _: LessThan |
           _: LessThanEquals |
           _: GreaterThan |
           _: GreaterThanEquals |
           _: Not                           =>
        val thn = ch.getFreshLabel(CodeGenerator.Then)
        val els = ch.getFreshLabel(CodeGenerator.Else)
        val after = ch.getFreshLabel(CodeGenerator.After)
        branch(expression, Label(thn), Label(els))
        ch << Label(thn)
        ch << Ldc(1)
        ch << Goto(after)
        ch << Label(els)
        ch << Ldc(0)
        ch << Label(after)
      case Plus(lhs, rhs)                   =>
        val argTypes = (lhs.getType, rhs.getType)
        argTypes match {
          case (TString, _) | (_, TString)       =>
            def methSignature(expr: ExprTree) = {
              val arg = expr.getType match {
                case TObject(tpe) => "L" + CodeGenerator.JavaObject + ";"
                case _            => expr.getType.byteCodeName
              }
              "(" + arg + ")L" + CodeGenerator.JavaStringBuilder + ";"
            }
            ch << DefaultNew(CodeGenerator.JavaStringBuilder)
            compileExpr(lhs)
            ch << InvokeVirtual(CodeGenerator.JavaStringBuilder, "append", methSignature(lhs))
            compileExpr(rhs)
            ch << InvokeVirtual(CodeGenerator.JavaStringBuilder, "append", methSignature(rhs))
            ch << InvokeVirtual(CodeGenerator.JavaStringBuilder, "toString", "()L" + CodeGenerator.JavaString + ";")
          case (_: TObject, _) | (_, _: TObject) =>
            compileExpr(lhs)
            compileExpr(rhs)
            compileOperatorCall(ch, expression, argTypes)
          case (TDouble, _) | (_, TDouble)       =>
            compileExpr(lhs)
            lhs.getType.codes.toDouble(ch)
            compileExpr(rhs)
            rhs.getType.codes.toDouble(ch)
            ch << DADD
          case (TFloat, _) | (_, TFloat)         =>
            compileExpr(lhs)
            lhs.getType.codes.toFloat(ch)
            compileExpr(rhs)
            rhs.getType.codes.toFloat(ch)
            ch << FADD
          case (TLong, _) | (_, TLong)           =>
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
      case BinaryOperator(lhs, rhs)         =>
        compileExpr(lhs)

        val argTypes = (lhs.getType, rhs.getType)
        argTypes match {
          case (_: TObject, _) | (_, _: TObject) =>
            compileExpr(rhs)
            compileOperatorCall(ch, expression, argTypes)
            return
          case (TDouble, _) | (_, TDouble)       =>
            lhs.getType.codes.toDouble(ch)
            compileExpr(rhs)
            rhs.getType.codes.toDouble(ch)
          case (TFloat, _) | (_, TFloat)         =>
            lhs.getType.codes.toFloat(ch)
            compileExpr(rhs)
            rhs.getType.codes.toFloat(ch)
          case (TLong, _) | (_, TLong)           =>
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
      case LogicalOperator(lhs, rhs)        =>
        val argTypes = (lhs.getType, rhs.getType)
        argTypes match {
          case (_: TObject, _) | (_, _: TObject) =>
            compileExpr(lhs)
            compileExpr(rhs)
            compileOperatorCall(ch, expression, argTypes)
            return
          case (TLong, _) | (_, TLong)           =>
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
      case ShiftOperator(lhs, rhs)          =>
        compileExpr(lhs)

        val argTypes = (lhs.getType, rhs.getType)
        argTypes match {
          case (_: TObject, _) | (_, _: TObject) =>
            compileExpr(rhs)
            compileOperatorCall(ch, expression, argTypes)
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
      case Assign(id, expr)                 =>
        store(id, () => {
          compileExpr(expr)
          val codes = expr.getType.codes
          id.getType match {
            case _: TObject | _: TArray | TString =>
            case TDouble                          => codes.toDouble(ch)
            case TFloat                           => codes.toFloat(ch)
            case TLong                            => codes.toLong(ch)
            case _                                => codes.toInt(ch)
          }
        }, duplicate)
      case ArrayAssign(arr, index, expr)     =>
        compileExpr(arr)
        compileExpr(index)
        compileExpr(expr)

        arr.getType match {
          case obj @ TObject(classSymbol) =>
            if (duplicate)
              obj.codes.dup_x2(ch) // arrayref index value -> value arrayref index value
            classSymbol.lookupOperator(expression, List(index.getType, expr.getType)) match {
              case Some(operatorSymbol) =>
                val className = classSymbol.name
                ch << InvokeVirtual(className, operatorSymbol.methodName, operatorSymbol.signature)
            }
          case TArray(arrayType)          =>
            val idCodes = arrayType.codes
            val exprCodes = expr.getType.codes
            def convertType() = arrayType match {
              case _: TObject | TString =>
              case TDouble              => exprCodes.toDouble(ch)
              case TFloat               => exprCodes.toFloat(ch)
              case TLong                => exprCodes.toLong(ch)
              case _                    => exprCodes.toInt(ch)
            }

            convertType()
            if (duplicate)
              idCodes.dup_x2(ch) // arrayref index value -> value arrayref index value
            idCodes.arrayStore(ch)
        }
      case Instance(expr, id)               =>
        compileExpr(expr)
        ch << InstanceOf(id.value)
      case As(expr, tpe)                    =>
        compileExpr(expr)
        ch << CheckCast(tpe.name)
      case ArrayRead(id, index)             =>
        compileExpr(id)
        compileExpr(index)
        id.getType match {
          case TObject(classSymbol) =>
            classSymbol.lookupOperator(expression, List(index.getType)) match {
              case Some(operatorSymbol) =>
                val className = classSymbol.name
                ch << InvokeVirtual(className, operatorSymbol.methodName, operatorSymbol.signature)
            }
          case TArray(arrayType)    =>
            arrayType.codes.arrayLoad(ch)
        }
      case ArrayLength(arr)                 =>
        compileExpr(arr)
        ch << ARRAYLENGTH
      case FieldRead(obj, id)               =>
        val className = obj.getType.asInstanceOf[TObject].classSymbol.name

        if (isStaticCall(obj) || isStatic(id)) {
          ch << GetStatic(className, id.value, id.getType.byteCodeName)
        } else {
          compileExpr(obj)
          ch << GetField(className, id.value, id.getType.byteCodeName)
        }
      case FieldAssign(obj, id, expr)       =>
        val fieldType = id.getType
        val exprCodes = expr.getType.codes
        def convertType() = fieldType match {
          case _: TObject | TString =>
          case TDouble              => exprCodes.toDouble(ch)
          case TFloat               => exprCodes.toFloat(ch)
          case TLong                => exprCodes.toLong(ch)
          case _                    => exprCodes.toInt(ch)
        }

        val static = isStaticCall(obj) || isStatic(id)

        val className = obj.getType.asInstanceOf[TObject].classSymbol.name
        if (!static)
          compileExpr(obj)
        compileExpr(expr)
        convertType()
        if (duplicate)
          id.getType.codes.dup_x1(ch) // ref value -> value ref value
        if (static) ch << PutStatic(className, id.value, id.getType.byteCodeName)
        else ch << PutField(className, id.value, id.getType.byteCodeName)
      case mc @ MethodCall(obj, meth, args) =>
        val className = obj.getType.asInstanceOf[TObject].classSymbol.name
        val signature = meth.getSymbol.asInstanceOf[MethodSymbol].signature
        if (isStaticCall(obj) || isStatic(meth)) {
          args.foreach(compileExpr(_))
          ch << InvokeStatic(className, meth.value, signature)
        } else {
          compileExpr(obj)
          args.foreach(compileExpr(_))
          ch << InvokeVirtual(className, meth.value, signature)
        }

        if (!duplicate && mc.getType != TUnit)
          ch << POP
      case newArray@ast.Trees.NewArray(tpe, sizes)    =>
        sizes.foreach(compileExpr(_))
        if(newArray.dimension == 1)
          tpe.getType.codes.newArray(ch)
        else
          ch << NewMultidimensionalArray(newArray.getType.byteCodeName, newArray.dimension)
      case ast.Trees.New(tpe, args)         =>
        val codes = tpe.getType.codes
        val obj = if (tpe.value == "Object") CodeGenerator.JavaObject else tpe.value
        ch << cafebabe.AbstractByteCodes.New(obj)
        codes.dup(ch)
        args.foreach(compileExpr(_))

        val signature = "(" + args.map(_.getType.byteCodeName).mkString + ")V"
        ch << InvokeSpecial(obj, CodeGenerator.ConstructorName, signature)
      case Negation(expr)                   =>
        compileExpr(expr)
        expr.getType match {
          case x: TObject => compileOperatorCall(ch, expression, x)
          case _          => expr.getType.codes.negation(ch)
        }
      case LogicNot(expr)                   =>
        compileExpr(expr)
        expr.getType match {
          case x: TObject => compileOperatorCall(ch, expression, x)
          case _          =>
            ch << Ldc(-1)
            expr.getType.codes.xor(ch)
        }
      case PreIncrement(id)                 => compileIncrementDecrement(isPre = true, isIncrement = true, id)
      case PreDecrement(id)                 => compileIncrementDecrement(isPre = true, isIncrement = false, id)
      case PostIncrement(id)                => compileIncrementDecrement(isPre = false, isIncrement = true, id)
      case PostDecrement(id)                => compileIncrementDecrement(isPre = false, isIncrement = false, id)
      case Ternary(condition, thn, els)     =>
        val thnLabel = ch.getFreshLabel(CodeGenerator.Then)
        val elsLabel = ch.getFreshLabel(CodeGenerator.Else)
        val afterLabel = ch.getFreshLabel(CodeGenerator.After)

        branch(condition, Label(thnLabel), Label(elsLabel))
        ch << Label(thnLabel)
        compileExpr(thn)
        ch << Goto(afterLabel)
        ch << Label(elsLabel)
        compileExpr(els)
        ch << Label(afterLabel)
    }

    def compileIncrementDecrement(isPre: Boolean, isIncrement: Boolean, id: Identifier) = {
      val isLocal = variableMap.contains(id.value)
      val codes = id.getType.codes

      id.getType match {
        case TInt if isLocal =>
          if (!isPre && duplicate) load(id)

          ch << IInc(variableMap(id.value), if (isIncrement) 1 else -1)

          if (isPre && duplicate) load(id)
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
    }
  }

  def branch(expression: ExprTree, thn: Label, els: Label): Unit = expression match {
    case Not(expr)                        =>
      expr.getType match {
        case x: TObject =>
          compileExpr(expr)
          compileOperatorCall(ch, expression, x)
          ch << IfEq(els.id) << Goto(thn.id)
        case _          =>
          branch(expr, els, thn)
      }
    case True()                           => ch << Goto(thn.id)
    case False()                          => ch << Goto(els.id)
    case And(lhs, rhs)                    =>
      val next = Label(ch.getFreshLabel(CodeGenerator.Next))
      branch(lhs, next, els)
      ch << next
      branch(rhs, thn, els)
    case Or(lhs, rhs)                     =>
      val next = Label(ch.getFreshLabel(CodeGenerator.Next))
      branch(lhs, thn, next)
      ch << next
      branch(rhs, thn, els)
    case id @ Identifier(value)           =>
      load(id)
      ch << IfEq(els.id) << Goto(thn.id)
    case Instance(expr, id)               =>
      compileExpr(expr)
      ch << Ldc(1)
      ch << InstanceOf(id.value) << If_ICmpEq(thn.id) << Goto(els.id)
    case ComparisonOperator(lhs, rhs)     =>
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
    case EqualsOperator(lhs, rhs)         =>
      def comparison(codes: CodeMap) = {
        expression match {
          case _: Equals    => codes.cmpEq(ch, thn.id)
          case _: NotEquals => codes.cmpNe(ch, thn.id)
        }
      }

      val argTypes = (lhs.getType, rhs.getType)
      compileExpr(lhs)
      argTypes match {
        case (_, objectType: TObject)        =>
          compileExpr(rhs)
          if (compileOperatorCall(ch, expression, argTypes))
            ch << IfNe(thn.id)
          else
            comparison(objectType.codes) // If no operator is defined, compare by reference
        case (objectType: TObject, _)        =>
          compileExpr(rhs)
          if (compileOperatorCall(ch, expression, argTypes))
            ch << IfEq(thn.id)
          else
            comparison(objectType.codes) // If no operator is defined, compare by reference
        case (_, _: TArray) | (_: TArray, _) =>
          compileExpr(rhs)
          comparison(ArrayCodeMap)
        case (TString, _) | (_, TString)     =>
          compileExpr(rhs)
          comparison(StringCodeMap)
        case (TDouble, _) | (_, TDouble)     =>
          lhs.getType.codes.toDouble(ch)
          compileExpr(rhs)
          rhs.getType.codes.toDouble(ch)
          comparison(DoubleCodeMap)
        case (TFloat, _) | (_, TFloat)       =>
          lhs.getType.codes.toFloat(ch)
          compileExpr(rhs)
          rhs.getType.codes.toFloat(ch)
          comparison(FloatCodeMap)
        case (TLong, _) | (_, TLong)         =>
          lhs.getType.codes.toLong(ch)
          compileExpr(rhs)
          rhs.getType.codes.toLong(ch)
          comparison(LongCodeMap)
        case _                               =>
          lhs.getType.codes.toInt(ch)
          compileExpr(rhs)
          rhs.getType.codes.toInt(ch)
          comparison(IntCodeMap)
      }
      ch << Goto(els.id)
    case mc @ MethodCall(obj, meth, args) =>
      compileExpr(mc)
      ch << IfEq(els.id) << Goto(thn.id)
  }

  private def store(id: Identifier, put: () => Unit, duplicate: Boolean = false): CodeHandler = {
    val name = id.value
    val tp = id.getType
    val codes = tp.codes

    if (variableMap.contains(name)) {
      val id = variableMap(name)
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
    val name = id.value
    val tpe = id.getType
    if (variableMap.contains(name)) {
      val id = variableMap(name)
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

object CodeGenerator {

  val ConstructorName = "<init>"

  /* Java classes used by compiler */
  val JavaStringBuilder = "java/lang/StringBuilder"
  val JavaString = "java/lang/String"
  val JavaSystem = "java/lang/System"
  val JavaPrintStream = "java/io/PrintStream"
  val JavaObject = "java/lang/Object"


  /* Labels */
  val Then = "then"
  val Else = "else"
  val After = "after"
  val Body = "body"
  val Next = "next"

}