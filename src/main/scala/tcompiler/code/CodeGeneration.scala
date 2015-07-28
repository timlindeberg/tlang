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


  val CONSTRUCTOR_NAME = "<init>"

  /* Java classes used by compiler */
  val STRING_BUILDER = "java/lang/StringBuilder"
  val STRING         = "java/lang/String"
  val SYSTEM         = "java/lang/System"
  val PRINT_STREAM   = "java/io/PrintStream"
  val OBJECT         = "java/lang/Object"


  /* Labels */
  val THEN  = "then"
  val ELSE  = "else"
  val AFTER = "after"
  val BODY  = "body"
  val NEXT  = "next"

  /* Types */
  val T_INT  = 10
  val T_BOOL = 4


  def run(ctx: Context)(prog: Program): Unit = {

    import ctx.reporter._

    class CodeGenerator(val ch: CodeHandler, className: String, variableMap: mutable.HashMap[String, Int]) {

      def store(id: Identifier, put: () => Unit, duplicate: Boolean = false): CodeHandler = {
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
          ch << ArgLoad(0) // put this-reference on stack
          put()
          if (duplicate)
            codes.dup_x1(ch) // this value -> value this value
          ch << PutField(className, name, tp.byteCodeName)
        }
      }

      def load(id: Identifier): CodeHandler = load(id.value, id.getType)

      def load(name: String, tpe: Type): CodeHandler =
        if (variableMap.contains(name)) {
          val id = variableMap(name)
          tpe.codes.load(ch, id)
        } else {
          ch << ArgLoad(0)
          ch << GetField(className, name, tpe.byteCodeName)
        }

      def compileStat(statement: StatTree): Unit = {
        putLineNumber(ch, statement.line)
        statement match {
          case Block(stats)                     => stats.foreach(compileStat)
          case If(expr, thn, els)               =>
            val thnLabel = ch.getFreshLabel(THEN)
            val elsLabel = ch.getFreshLabel(ELSE)
            val afterLabel = ch.getFreshLabel(AFTER)

            branch(expr, Label(thnLabel), Label(elsLabel))
            ch << Label(thnLabel)
            compileStat(thn)
            ch << Goto(afterLabel)
            ch << Label(elsLabel)
            if (els.isDefined) compileStat(els.get)
            ch << Label(afterLabel)
          case While(expr, stat)                =>
            val bodyLabel = ch.getFreshLabel(BODY)
            val afterLabel = ch.getFreshLabel(AFTER)
            branch(expr, Label(bodyLabel), Label(afterLabel))
            ch << Label(bodyLabel)
            compileStat(stat)
            branch(expr, Label(bodyLabel), Label(afterLabel))
            ch << Label(afterLabel)
          case For(init, condition, post, stat) =>
            val bodyLabel = ch.getFreshLabel(BODY)
            val afterLabel = ch.getFreshLabel(AFTER)
            init.foreach(compileStat)
            branch(condition, Label(bodyLabel), Label(afterLabel))
            ch << Label(bodyLabel)
            compileStat(stat)
            post.foreach(compileStat)
            branch(condition, Label(bodyLabel), Label(afterLabel))
            ch << Label(afterLabel)
          case PrintStatement(expr)             =>
            ch << GetStatic(SYSTEM, "out", "L" + PRINT_STREAM + ";")
            compileExpr(expr)
            val arg = expr.getType match {
              case _: TObject => "L" + OBJECT + ";" // Call System.out.println(Object) for all other types
              case _          => expr.getType.byteCodeName
            }
            val funcName = statement match {
              case _: Print   => "print"
              case _: Println => "println"
            }
            ch << InvokeVirtual(PRINT_STREAM, funcName, "(" + arg + ")V")
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

      def compileOperatorCall(ch: CodeHandler, expr: ExprTree, arg: Type): Boolean = compileOperatorCall(ch, arg, expr, List(arg))
      def compileOperatorCall(ch: CodeHandler, expr: ExprTree, args: (Type, Type)): Boolean = {
        val argsList = List(args._1, args._2)
        compileOperatorCall(ch, args._1, expr, argsList) || compileOperatorCall(ch, args._2, expr, argsList)
      }

      def compileExpr(expression: ExprTree, duplicate: Boolean = true): Unit = {
        putLineNumber(ch, expression.line)
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
          case id @ ClassIdentifier(value, _)   => load(id.value, id.getType)
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
            val thn = ch.getFreshLabel(THEN)
            val els = ch.getFreshLabel(ELSE)
            val after = ch.getFreshLabel(AFTER)
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
                    case TObject(tpe) => "L" + OBJECT + ";"
                    case _            => expr.getType.byteCodeName
                  }
                  "(" + arg + ")L" + STRING_BUILDER + ";"
                }
                ch << DefaultNew(STRING_BUILDER)
                compileExpr(lhs)
                ch << InvokeVirtual(STRING_BUILDER, "append", methSignature(lhs))
                compileExpr(rhs)
                ch << InvokeVirtual(STRING_BUILDER, "append", methSignature(rhs))
                ch << InvokeVirtual(STRING_BUILDER, "toString", "()L" + STRING + ";")
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
            }, duplicate = duplicate)
          case ArrayAssign(id, index, expr)     =>
            val arrayType = id.getType.asInstanceOf[TArray].tpe
            val idCodes = arrayType.codes
            val exprCodes = expr.getType.codes
            def convertType() = arrayType match {
              case _: TObject | TString =>
              case TDouble              => exprCodes.toDouble(ch)
              case TFloat               => exprCodes.toFloat(ch)
              case TLong                => exprCodes.toLong(ch)
              case _                    => exprCodes.toInt(ch)
            }
            load(id)
            compileExpr(index)
            compileExpr(expr)
            convertType()
            if (duplicate)
              idCodes.dup_x2(ch) // arrayref index value -> value arrayref index value
            idCodes.arrayStore(ch)
          case Instance(expr, id)               =>
            compileExpr(expr)
            ch << InstanceOf(id.value)
          case As(expr, tpe)                    =>
            compileExpr(expr)
            ch << CheckCast(tpe.name)
          case ArrayRead(arr, index)            =>
            compileExpr(arr)
            compileExpr(index)
            val tpe = arr.getType.asInstanceOf[TArray].tpe
            tpe.codes.arrayLoad(ch)
          case ArrayLength(arr)                 =>
            compileExpr(arr)
            ch << ARRAYLENGTH
          case FieldRead(obj, id)               =>
            compileExpr(obj)
            val name = obj.getType.asInstanceOf[TObject].classSymbol.name
            ch << GetField(name, id.value, id.getType.byteCodeName)
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

            compileExpr(obj) // ref
            compileExpr(expr) // ref value
            convertType()
            if (duplicate)
              id.getType.codes.dup_x1(ch) // ref value -> value ref value

            val name = obj.getType.asInstanceOf[TObject].classSymbol.name
            ch << PutField(name, id.value, id.getType.byteCodeName)
          case mc @ MethodCall(obj, meth, args) =>
            compileExpr(obj)
            args.foreach(compileExpr(_))
            val className = obj.getType.asInstanceOf[TObject].classSymbol.name
            val signature = meth.getSymbol.asInstanceOf[MethodSymbol].signature
            ch << InvokeVirtual(className, meth.value, signature)
            if (!duplicate && mc.getType != TUnit)
              ch << POP
          case ast.Trees.NewArray(tpe, size)    =>
            compileExpr(size)
            tpe.getType.codes.newArray(ch)

          case ast.Trees.New(tpe, args)     =>
            val codes = tpe.getType.codes
            val obj = if (tpe.value == "Object") OBJECT else tpe.value
            ch << cafebabe.AbstractByteCodes.New(obj)
            codes.dup(ch)
            args.foreach(compileExpr(_))

            val signature = "(" + args.map(_.getType.byteCodeName).mkString + ")V"
            ch << InvokeSpecial(obj, CONSTRUCTOR_NAME, signature)
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
            // TODO: Fix post/pre increment for objects. Currently they work the same way.
          case PreDecrement(id)             =>
            val codes = id.getType.codes
            store(id, () => {
              load(id)
              id.getType match {
                case x: TObject => compileOperatorCall(ch, expression, x)
                case _          =>
                  codes.one(ch)
                  codes.sub(ch)
              }
            }, duplicate = duplicate)
          case PreIncrement(id)             =>
            val codes = id.getType.codes
            store(id, () => {
              load(id)
              id.getType match {
                case x: TObject => compileOperatorCall(ch, expression, x)
                case _          =>
                  codes.one(ch)
                  codes.add(ch)
              }
            }, duplicate = duplicate)
          case PostDecrement(id)            =>
            val codes = id.getType.codes
            store(id, () => {
              load(id)
              id.getType match {
                case x: TObject =>
                  compileOperatorCall(ch, expression, x)
                  x.codes.dup(ch)
                case _          =>
                  if (duplicate)
                    codes.dup_x1(ch)
                  codes.one(ch)
                  codes.sub(ch)
              }
            })
          case PostIncrement(id)            =>
            val codes = id.getType.codes
            store(id, () => {
              load(id)
              id.getType match {
                case x: TObject => compileOperatorCall(ch, expression, x)
                  x.codes.dup(ch)
                case _          =>
                  if (duplicate)
                    codes.dup_x1(ch)
                  codes.one(ch)
                  codes.add(ch)
              }
            })
          case Ternary(condition, thn, els) =>
            val thnLabel = ch.getFreshLabel(THEN)
            val elsLabel = ch.getFreshLabel(ELSE)
            val afterLabel = ch.getFreshLabel(AFTER)

            branch(condition, Label(thnLabel), Label(elsLabel))
            ch << Label(thnLabel)
            compileExpr(thn)
            ch << Goto(afterLabel)
            ch << Label(elsLabel)
            compileExpr(els)
            ch << Label(afterLabel)
        }
      }

      private def branch(expression: ExprTree, thn: Label, els: Label): Unit = expression match {
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
          val next = Label(ch.getFreshLabel(NEXT))
          branch(lhs, next, els)
          ch << next
          branch(rhs, thn, els)
        case Or(lhs, rhs)                     =>
          val next = Label(ch.getFreshLabel(NEXT))
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

      var prevLine: Int = 0
      private def putLineNumber(ch: CodeHandler, line: Int) =
        if (line != prevLine) {
          prevLine = line
          ch << LineNumber(line)
        }
    }



    /** Writes the proper .class file in a given directory. An empty string for dir is equivalent to "./". */
    def generateClassFile(sourceName: String, ct: ClassDecl, dir: String): Unit = {
      val sym = ct.getSymbol
      val classFile = new ClassFile(sym.name, sym.parent.map(_.name))
      classFile.setSourceFile(sourceName)
      sym.members.foreach { case (name, varSymbol) =>
        classFile.addField(varSymbol.getType.byteCodeName, name).setFlags(getAccessFlag(varSymbol.access))
      }

      var hasConstructor = false
      ct.methods.foreach { mt =>
        val methSymbol = mt.getSymbol

        val methodHandle = mt match {
          case mt: MethodDecl       =>
            val argTypes = methSymbol.argList.map(_.getType.byteCodeName).mkString
            classFile.addMethod(methSymbol.getType.byteCodeName, methSymbol.name, argTypes)
          case con: ConstructorDecl =>
            hasConstructor = true
            generateConstructor(con, classFile, ct)
          case op: OperatorDecl     =>
            val argTypes = methSymbol.argList.map(_.getType.byteCodeName).mkString
            val operatorSymbol = op.getSymbol.asInstanceOf[OperatorSymbol]
            val methodHandle = classFile.addMethod(methSymbol.getType.byteCodeName, operatorSymbol.methodName, argTypes)
            methodHandle
        }
        mt match {
          case _: OperatorDecl => methodHandle.setFlags((Flags.METHOD_ACC_STATIC | getAccessFlag(mt.access)).asInstanceOf[Short])
          case _               => methodHandle.setFlags(getAccessFlag(mt.access))
        }
        generateMethod(methodHandle.codeHandler, mt)
      }

      if (!hasConstructor)
        classFile.addDefaultConstructor

      val file = getFilePath(dir, sym)
      classFile.writeToFile(file)
    }

    def getAccessFlag(access: Accessability) = access match {
      case Public    => Flags.FIELD_ACC_PUBLIC
      case Private   => Flags.FIELD_ACC_PRIVATE
      case Protected => Flags.FIELD_ACC_PROTECTED
    }

    def getFilePath(outDir: String, sym: ClassSymbol): String = {
      val split = sym.name.split("/")
      val packageDir = split.take(split.size - 1).mkString("/")
      val filePath = outDir + packageDir
      val f = new File(filePath)
      if (!f.getAbsoluteFile.exists()) {
        if (!f.mkdirs())
          error("Could not create output directory \'" + f.getAbsolutePath + "\'.")
      }
      outDir + sym.name + ".class"
    }





    def generateMethod(ch: CodeHandler, mt: FuncTree): Unit = {
      val methSym = mt.getSymbol
      val variableMap = mutable.HashMap[String, Int]()

      val startIndex = mt match {
        case _: OperatorDecl => 0
        case _               => 1
      }
      mt.args.zipWithIndex.foreach {
        case (arg, i) => variableMap(arg.getSymbol.name) = i + startIndex
      }

      val codeGenerator = new CodeGenerator(ch, methSym.classSymbol.name, variableMap)
      initializeLocalVariables(mt, codeGenerator, variableMap)

      mt.stats.foreach(codeGenerator.compileStat)

      addReturnStatement(ch, mt)
      ch.freeze
    }

    def addReturnStatement(ch: CodeHandler, mt: FuncTree) =
      ch.peek match {
        case ARETURN | IRETURN | RETURN | DRETURN | FRETURN | LRETURN =>
        case _                                                        =>
          mt match {
            case mt: MethodDecl        =>
              val tpe = mt.retType.getType
              tpe match {
                case TUnit => ch << RETURN
                case _     =>
                  tpe.codes.defaultConstant(ch)
                  tpe.codes.ret(ch)
              }
            case cons: ConstructorDecl => ch << RETURN
          }
      }

    def generateConstructor(con: ConstructorDecl, classFile: ClassFile, ct: ClassDecl): MethodHandler = {
      val argTypes = con.getSymbol.argList.map(_.getType.byteCodeName).mkString
      val mh = classFile.addConstructor(argTypes)
      val ch = mh.codeHandler

      // Initialize fields after constructor
      val codeGenerator = new CodeGenerator(ch, con.getSymbol.classSymbol.name, mutable.HashMap())
      ct.vars.foreach {
        case VarDecl(varTpe, id, Some(expr), _) =>
          ch << ArgLoad(0) // put this-reference on stack
          codeGenerator.compileExpr(expr)
          ch << PutField(con.getSymbol.classSymbol.name, id.value, varTpe.getType.byteCodeName)
        case _                                  =>
      }

      addSuperCall(mh, ct)
      mh
    }


    def addSuperCall(mh: MethodHandler, ct: ClassDecl) = {
      val superClassName = ct.parent match {
        case Some(name) => name.value
        case None       => OBJECT
      }

      mh.codeHandler << ALOAD_0
      mh.codeHandler << InvokeSpecial(superClassName, CONSTRUCTOR_NAME, "()V")
    }

    def initializeLocalVariables(mt: FuncTree, codeGenerator: CodeGenerator, variableMap: mutable.HashMap[String, Int]) = {
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
      // ch.print
      ch.freeze
    }
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
}
