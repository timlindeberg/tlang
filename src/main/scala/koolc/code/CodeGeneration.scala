package koolc
package code

import ast.Trees._
import analyzer.Symbols._
import analyzer.Types._
import cafebabe._
import cafebabe.ByteCodes._
import utils._
import scala.collection.mutable.HashMap
import cafebabe.AbstractByteCodes._

object CodeGeneration extends Pipeline[Program, Unit] {

  /* Java classes used by compiler */
  val STRING_BUILDER = "java/lang/StringBuilder"
  val STRING = "java/lang/String"
  val SYSTEM = "java/lang/System"
  val PRINT_STREAM = "java/io/PrintStream"
  val OBJECT = "java/lang/Object"


  /* Labels */
  val THEN = "then"
  val ELSE = "else"
  val AFTER = "after"
  val BODY = "body"
  val NEXT = "next"

  /* Types */
  val T_INT = 10
  val CONSTRUCTOR_NAME = "<init>"

  def run(ctx: Context)(prog: Program): Unit = {

    def getIntOrReference[T](tp: Type, Iret: T, Aret: T) = tp match {
      case TBool | TInt => Iret
      case _            => Aret
    }

    var varMap = new HashMap[String, Int]

    class StatementCompiler(ch: CodeHandler, cn: String) {

      def store(id: Identifier, put: () => Unit): Unit = {
        val name = id.value
        val tp = id.getType

        if (varMap.contains(name)) {
          val id = varMap(name)
          put()
          ch << getIntOrReference(tp, IStore(id), AStore(id))
        } else {
          ch << ArgLoad(0) // put this-reference on stack
          put()
          ch << PutField(cn, name, tp.byteCodeName)
        }
      }

      def load(id: Identifier): Unit = load(id.value, id.getType)

      def load(name: String, tpe: Type): Unit =
        if (varMap.contains(name)) {
          val id = varMap(name)
          ch << getIntOrReference(tpe, ILoad(id), ALoad(id))
        } else {
          ch << ArgLoad(0)
          ch << GetField(cn, name, tpe.byteCodeName)
        }

      def compileStat(stat: StatTree): Unit = {
        ch << LineNumber(stat.line)
        stat match {
          case Block(stats) => stats.foreach(compileStat)
          case If(expr, thn, els) =>
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
          case While(expr, stat) =>
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
          case Println(expr) =>
            ch << GetStatic(SYSTEM, "out", "L" + PRINT_STREAM + ";")
            compileExpr(expr)
            val arg = getIntOrReference(expr.getType, expr.getType.byteCodeName, "L" + OBJECT + ";")
            ch << InvokeVirtual(PRINT_STREAM, "println", "(" + arg + ")V")
          case Assign(id, expr) =>
            store(id, () => compileExpr(expr))
          case a @ Assignment(id, expr) =>
            store(id, () => {
              load(id)
              compileExpr(expr)
              ch << (a match {
                case _: PlusAssign       => IADD
                case _: MinusAssign      => ISUB
                case _: MulAssign        => IMUL
                case _: DivAssign        => IDIV
                case _: ModAssign        => IREM
                case _: AndAssign        => IAND
                case _: OrAssign         => IOR
                case _: XorAssign        => IXOR
                case _: LeftShiftAssign  => ISHL
                case _: RightShiftAssign => ISHR
              })
            })
          case ArrayAssign(id, index, expr) =>
            load(id)
            compileExpr(index)
            compileExpr(expr)
            ch << IASTORE
          case mc @ MethodCall(obj, meth, args) =>
            compileExpr(obj)
            args.foreach(compileExpr)
            val methArgList = meth.getSymbol.asInstanceOf[MethodSymbol].argList
            val argTypes = methArgList.map(_.getType.byteCodeName).mkString
            val signature = "(" + argTypes + ")" + mc.getType.byteCodeName
            val name = obj.getType.asInstanceOf[TObject].classSymbol.name
            ch << InvokeVirtual(name, meth.value, signature)
            if(mc.getType != TUnit)
              ch << POP
          case Return(Some(expr)) =>
              compileExpr(expr)
              ch << getIntOrReference(expr.getType, IRETURN, ARETURN)
          case Return(None) =>
              ch << RETURN
          case PreDecrement(id @ Identifier(name)) =>
            load(id)
            store(id, () => {
              load(id)
              ch << Ldc(1) << ISUB
            })
          case PreIncrement(id) =>
            store(id, () => {
              load(id)
              ch << Ldc(1) << IADD
            } )
          case PostDecrement(id) =>
            store(id, () => {
              load(id)
              ch << Ldc(1) << ISUB
            })
          case PostIncrement(id) =>
            store(id, () => {
              load(id)
              ch << Ldc(1) << IADD
            })
        }
      }

      def branch(expr: ExprTree, thn: Label, els: Label): Unit = expr match {
        case Not(expr) => branch(expr, els, thn)
        case True() => ch << Goto(thn.id)
        case False() => ch << Goto(els.id)
        case And(lhs, rhs) =>
          val next = Label(ch.getFreshLabel(NEXT))
          branch(lhs, next, els)
          ch << next
          branch(rhs, thn, els)
        case Or(lhs, rhs) =>
          val next = Label(ch.getFreshLabel(NEXT))
          branch(lhs, thn, next)
          ch << next
          branch(rhs, thn, els)
        case id@Identifier(value) =>
          load(id)
          ch << IfEq(els.id)
          ch << Goto(thn.id)
        case c @ Comparison(lhs, rhs) =>
          compileExpr(lhs)
          compileExpr(rhs)
          ch << (c match {
            case _: LessThan          => If_ICmpLt(thn.id)
            case _: LessThanEquals    => If_ICmpLe(thn.id)
            case _: GreaterThan       => If_ICmpGt(thn.id)
            case _: GreaterThanEquals => If_ICmpGe(thn.id)
            case _: Equals            => getIntOrReference(lhs.getType, If_ICmpEq(thn.id), If_ACmpEq(thn.id))
            case _: NotEquals         => getIntOrReference(lhs.getType, If_ICmpNe(thn.id), If_ACmpNe(thn.id))
          })
          ch << Goto(els.id)
        case mc@MethodCall(obj, meth, args) =>
          compileExpr(mc)
          ch << IfEq(els.id)
          ch << Goto(thn.id)
        case _ => throw new UnsupportedOperationException(expr.toString)
      }

      def compileExpr(expr: ExprTree): Unit = {
        ch << LineNumber(expr.line)
        expr match {
          case _:And | _:Or | _:Equals | _:NotEquals | _:LessThan |
               _:LessThanEquals | _:GreaterThan | _:GreaterThanEquals | _:Not  =>
            val thn = ch.getFreshLabel(THEN)
            val els = ch.getFreshLabel(ELSE)
            val after = ch.getFreshLabel(AFTER)
            branch(expr, Label(thn), Label(els))
            ch << Label(thn)
            ch << Ldc(1)
            ch << Goto(after)
            ch << Label(els)
            ch << Ldc(0)
            ch << Label(after)
          case expr@Plus(lhs, rhs) => expr.getType match {
            case TInt =>
              compileExpr(lhs)
              compileExpr(rhs)
              ch << IADD
            case TString =>
              def methSignature(expr: ExprTree) = "(" + expr.getType.byteCodeName + ")L" + STRING_BUILDER + ";"
              ch << DefaultNew(STRING_BUILDER)
              compileExpr(lhs)
              ch << InvokeVirtual(STRING_BUILDER, "append", methSignature(lhs))
              compileExpr(rhs)
              ch << InvokeVirtual(STRING_BUILDER, "append", methSignature(rhs))
              ch << InvokeVirtual(STRING_BUILDER, "toString", "()L" + STRING + ";")
            case _ => throw new UnsupportedOperationException(expr.toString)
          }
          case e @ MathBinaryExpr(lhs, rhs) =>
            compileExpr(lhs)
            compileExpr(rhs)
            ch << (e match {
              case _: Minus      => ISUB
              case _: LogicAnd   => IAND
              case _: LogicOr    => IOR
              case _: LogicXor   => IXOR
              case _: LeftShift  => ISHL
              case _: LeftShift  => ISHL
              case _: RightShift => ISHR
              case _: Times      => IMUL
              case _: Div        => IDIV
              case _: Modulo     => IREM
            })
          case ArrayRead(arr, index) =>
            compileExpr(arr)
            compileExpr(index)
            ch << IALOAD
          case ArrayLength(arr) =>
            compileExpr(arr)
            ch << ARRAYLENGTH
          case mc@MethodCall(obj, meth, args) =>
            compileExpr(obj)
            args.foreach(compileExpr)
            val methArgList = meth.getSymbol.asInstanceOf[MethodSymbol].argList
            val argTypes = methArgList.map(_.getType.byteCodeName).mkString
            val signature = "(" + argTypes + ")" + mc.getType.byteCodeName
            val name = obj.getType.asInstanceOf[TObject].classSymbol.name
            ch << InvokeVirtual(name, meth.value, signature)
          case NewIntArray(size) =>
            compileExpr(size)
            ch << NewArray(T_INT) // I?
          case True() => ch << Ldc(1)
          case False() => ch << Ldc(0)
          case IntLit(value) => ch << Ldc(value)
          case StringLit(value) => ch << Ldc(value)
          case id@Identifier(value) => load(id)
          case id@TypeIdentifier(value, _) => load(id.value, id.getType)
          case This() => ch << ArgLoad(0)
          case ast.Trees.New(tpe, args) =>
            val obj = if (tpe.value == "Object") OBJECT else tpe.value
            val argTypes = args.map(_.getType.byteCodeName).mkString
            val signature = "(" + argTypes + ")V"

            ch << cafebabe.AbstractByteCodes.New(obj)
            ch << DUP
            args.foreach(compileExpr)
            ch << InvokeSpecial(obj, CONSTRUCTOR_NAME, signature)
          case Negation(expr) =>
            compileExpr(expr)
            ch << INEG
          case LogicNot(expr) =>
            compileExpr(expr)
            ch << Ldc(-1) << IXOR
          case PreDecrement(id) =>
            store(id, () => {
              load(id)
              ch << Ldc(1) << ISUB << DUP
            })
          case PreIncrement(id) =>
            store(id, () => {
              load(id)
              ch << Ldc(1) << IADD << DUP
            })
          case PostDecrement(id) =>
            store(id, () => {
              load(id)
              ch << DUP << Ldc(1) << ISUB
            })
          case PostIncrement(id) =>
            store(id, () => {
              load(id)
              ch << DUP << Ldc(1) << IADD
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
          case _ =>
        }
      }
    }

    /** Writes the proper .class file in a given directory. An empty string for dir is equivalent to "./". */
    def generateClassFile(sourceName: String, ct: ClassDecl, dir: String): Unit = {
      val sym = ct.getSymbol
      val classFile = new ClassFile(sym.name, sym.parent.map(_.name))
      classFile.setSourceFile(sourceName)
      var hasConstructor = false

      sym.members.foreach { case (name, varSymbol) => classFile.addField(varSymbol.getType.byteCodeName, name) }

      ct.methods.foreach { mt =>
        val methSymbol = mt.getSymbol
        val argTypes = methSymbol.argList.map(_.getType.byteCodeName).mkString

        val methodHandle = mt match {
          case mt:  MethodDecl      => classFile.addMethod(methSymbol.getType.byteCodeName, methSymbol.name, argTypes)
          case con: ConstructorDecl =>
            hasConstructor = true
            val mh = classFile.addConstructor(argTypes)
            addSuperCall(mh, ct)
            mh
        }
        generateMethodCode(methodHandle.codeHandler, mt)
      }

      if(!hasConstructor)
        classFile.addDefaultConstructor

      classFile.writeToFile(if (dir.length == 0) "./" else dir + sym.name + ".class")
    }

    def addSuperCall(mh: MethodHandler, ct: ClassDecl) = {
      val superClassName = ct.parent match {
        case Some(name) => name.value
        case None       => OBJECT
      }

      mh.codeHandler << ALOAD_0
      mh.codeHandler << InvokeSpecial(superClassName, CONSTRUCTOR_NAME, "()V")
    }

    def generateMethodCode(ch: CodeHandler, mt: FuncTree): Unit = {
      val methSym = mt.getSymbol

      varMap = new HashMap[String, Int]
      mt.args.zipWithIndex.foreach {
        case (arg, i) => varMap(arg.getSymbol.name) = i + 1
      }

      initializeLocalVariables(mt, ch)

      val statementCompiler = new StatementCompiler(ch, methSym.classSymbol.name)
      mt.stats.foreach(statementCompiler.compileStat)

      // Always add a return at the end of the function in case one is missing
      mt match {
        case mt: MethodDecl => mt.retType.getType match {
          case TInt | TBool => ch << Ldc(0) << IRETURN
          case TUnit        => ch << RETURN
          case _            => ch << ACONST_NULL << ARETURN
        }
        case cons: ConstructorDecl => ch << RETURN
      }

      ch.freeze
    }

    def initializeLocalVariables(mt: FuncTree, ch: CodeHandler) = mt.vars foreach { variable =>
      val id = ch.getFreshVar
      varMap(variable.getSymbol.name) = id
      variable.getSymbol.getType match {
        case TInt | TBool => ch << Ldc(0) << IStore(id)
        case _ => ch << ACONST_NULL << AStore(id)
      }
    }

    def generateMainClassFile(sourceName: String, main: MainObject, dir: String) = {
      val mainClassFile = new ClassFile(prog.main.id.value, None)
      mainClassFile.setSourceFile(sourceName)
      mainClassFile.addDefaultConstructor
      generateMainMethodCode(mainClassFile.addMainMethod.codeHandler, main.stats, main.id.value)
      mainClassFile.writeToFile(dir + prog.main.id.value + ".class")
    }

    def generateMainMethodCode(ch: CodeHandler, stmts: List[StatTree], cname: String): Unit = {
      varMap = new HashMap[String, Int]
      stmts.foreach(new StatementCompiler(ch, cname).compileStat)
      ch << RETURN
      ch.freeze
    }

    val outDir = ctx.outDir.map(_.getPath + "/").getOrElse("./")

    val f = new java.io.File(outDir)
    if (!f.exists()) f.mkdirs()

    val sourceName = ctx.file.getName

    // output code
    prog.classes.foreach(generateClassFile(sourceName, _, outDir))
    generateMainClassFile(sourceName, prog.main, outDir)
  }

}
