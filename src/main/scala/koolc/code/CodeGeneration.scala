package koolc
package code

import ast.Trees._
import analyzer.Symbols._
import analyzer.Types._
import cafebabe._
import cafebabe.ByteCodes._
import utils._
import scala.collection.mutable.HashMap
import com.sun.org.apache.bcel.internal.generic.IF_ICMPLE
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

  def run(ctx: Context)(prog: Program): Unit = {
    import ctx.reporter._

    var varMap = new HashMap[String, Int]

    class CodeCompiler(ch: CodeHandler, cn: String){
      def store(expr: ExprTree, id: Identifier): Unit = {
        val name = id.value
        val tp = id.getType

        if (varMap.contains(name)) {
          compile(expr)
          ch << (tp match {
            case TBool | TInt => IStore(varMap(name))
            case _            => AStore(varMap(name))
          })
        } else {
          ch << ArgLoad(0) // put this-reference on stack
          compile(expr)
          ch << PutField(cn, name, tp.byteCodeName)
        }

      }

      def load(id: Identifier): Unit = {
        val name = id.value
        val tp = id.getType

        if (varMap.contains(name))
          ch << (tp match {
            case TBool | TInt => ILoad(varMap(name))
            case _            => ALoad(varMap(name))
          })
        else {
          ch << ArgLoad(0) // put this-reference on stack
          ch << GetField(cn, name, tp.byteCodeName)
        }
      }

      def compile(stat: StatTree): Unit = stat match {
        case Block(stats) => stats.foreach(compile)
        case If(expr, thn, els) =>
          val thnLabel = ch.getFreshLabel(THEN)
          val elsLabel = ch.getFreshLabel(ELSE)
          val afterLabel = ch.getFreshLabel(AFTER)

          branch(expr, Label(thnLabel), Label(elsLabel))
          ch << Label(thnLabel)
          compile(thn)
          ch << Goto(afterLabel)
          ch << Label(elsLabel)
          if (els.isDefined) compile(els.get)
          ch << Label(afterLabel)

        case While(expr, stat) =>
          val bodyLabel = ch.getFreshLabel(BODY)
          val afterLabel = ch.getFreshLabel(AFTER)
          branch(expr, Label(bodyLabel), Label(afterLabel))
          ch << Label(bodyLabel)
          compile(stat)
          branch(expr, Label(bodyLabel), Label(afterLabel))
          ch << Label(afterLabel)
        case Println(expr) =>
          ch << GetStatic(SYSTEM, "out", "L" + PRINT_STREAM + ";")
          compile(expr)
          val arg = expr.getType match {
            case TInt | TBool => expr.getType.byteCodeName
            case _            => "L" + OBJECT + ";"
          }
          ch << InvokeVirtual(PRINT_STREAM, "println", "(" + arg + ")V")
        case Assign(id, expr) =>
          store(expr, id)
        case ArrayAssign(id, index, expr) =>
          load(id)
          compile(index)
          compile(expr)
          ch << IASTORE
      }

      def branch(expr: ExprTree, thn: Label, els: Label): Unit = expr match {
        case Not(expr) => branch(expr, els, thn)
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
        case True() =>
          ch << Goto(thn.id)
        case False() =>
          ch << Goto(els.id)
        case id @ Identifier(value) =>
          load(id)
          ch << IfEq(els.id)
          ch << Goto(thn.id)
        case LessThan(lhs, rhs) =>
          compile(lhs)
          compile(rhs)
          ch << If_ICmpLt(thn.id)
          ch << Goto(els.id)
        case Equals(lhs, rhs) =>
          compile(lhs)
          compile(rhs)
          lhs.getType match {
            case TInt | TBool => ch << If_ICmpEq(thn.id)
            case _            => ch << If_ACmpEq(thn.id)
          }
          ch << Goto(els.id)
        case mc @ MethodCall(obj, meth, args) =>
          compile(mc)
          ch << IfEq(els.id)
          ch << Goto(thn.id)
      }

      def compile(expr: ExprTree): Unit = expr match {
        case And(_, _) | Or(_, _) | Equals(_, _) | LessThan(_, _) | Not(_) =>
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
        case True()  => ch << Ldc(1)
        case False() => ch << Ldc(0)
        case expr @ Plus(lhs, rhs) =>
          expr.getType match {
            case TInt => {
              compile(lhs)
              compile(rhs)
              ch << IADD
            }
            case TString => {
              ch << DefaultNew(STRING_BUILDER)
              compile(lhs)
              ch << InvokeVirtual(STRING_BUILDER, "append", "(" + lhs.getType.byteCodeName + ")L" + STRING_BUILDER + ";")
              compile(rhs)
              ch << InvokeVirtual(STRING_BUILDER, "append", "(" + rhs.getType.byteCodeName + ")L" + STRING_BUILDER + ";")
              ch << InvokeVirtual(STRING_BUILDER, "toString", "()L" + STRING + ";")
            }
            case _ => throw new UnsupportedOperationException(expr.toString)
          }

        case Minus(lhs, rhs) =>
          compile(lhs)
          compile(rhs)
          ch << ISUB
        case Times(lhs, rhs) =>
          compile(lhs)
          compile(rhs)
          ch << IMUL
        case Div(lhs, rhs) =>
          compile(lhs)
          compile(rhs)
          ch << IDIV
        case ArrayRead(arr, index) =>
          compile(arr)
          compile(index)
          ch << IALOAD
        case ArrayLength(arr) =>
          compile(arr)
          ch << ARRAYLENGTH
        case mc @ MethodCall(obj, meth, args) =>
          compile(obj)
          args.foreach(compile)
          val methArgList = meth.getSymbol.asInstanceOf[MethodSymbol].argList
          val argTypes = methArgList.map(_.getType.byteCodeName).mkString
          val signature = "(" + argTypes + ")" + mc.getType.byteCodeName
          val name = obj.getType.asInstanceOf[TObject].classSymbol.name
          ch << InvokeVirtual(name, meth.value, signature)
        case IntLit(value) =>
          ch << Ldc(value)
        case StringLit(value) =>
          ch << Ldc(value)
        case id @ Identifier(value) =>
          load(id)
        case This() =>
          ch << ArgLoad(0)
        case NewIntArray(size) =>
          compile(size)
          ch << NewArray(10) // I?
        case ast.Trees.New(tpe) =>
          ch << DefaultNew(tpe.value)
      }
    }

    /** Writes the proper .class file in a given directory. An empty string for dir is equivalent to "./". */
    def generateClassFile(sourceName: String, ct: ClassDecl, dir: String): Unit = {
      // TODO: Create code handler, save to files ...
      val sym = ct.getSymbol
      val classFile = new ClassFile(sym.name, sym.parent.map(_.name))
      classFile.setSourceFile(sourceName)
      classFile.addDefaultConstructor

      sym.members.foreach {
        case (name, varSymbol) =>
          classFile.addField(varSymbol.getType.byteCodeName, name)
      }

      ct.methods.foreach { mt =>
        val methSymbol = mt.getSymbol
        val argType = methSymbol.argList.map(_.getType.byteCodeName).mkString
        val mh = classFile.addMethod(methSymbol.getType.byteCodeName, methSymbol.name, argType)
        generateMethodCode(mh.codeHandler, mt)
      }

      classFile.writeToFile(if (dir.length == 0) "./" else dir + sym.name + ".class")
    }

    // a mapping from variable symbols to positions in the local variables
    // of the stack frame
    def generateMethodCode(ch: CodeHandler, mt: MethodDecl): Unit = {
      val methSym = mt.getSymbol

      varMap = new HashMap[String, Int]
      mt.args.zipWithIndex.foreach {
        case (arg, i) => varMap(arg.getSymbol.name) = i + 1
      }
      mt.vars foreach { variable =>
        varMap(variable.getSymbol.name) = ch.getFreshVar
        variable.getSymbol.getType match {
          case TInt | TBool => ch << Ldc(0) << IStore(varMap(variable.getSymbol.name))
          case _            => ch << ACONST_NULL << AStore(varMap(variable.getSymbol.name))
        }
      }
      val codeCompiler = new CodeCompiler(ch, methSym.classSymbol.name)
      mt.stats.foreach(codeCompiler.compile)
      codeCompiler.compile(mt.retExpr)

      ch << (mt.retType match {
        case IntType() | BooleanType() => IRETURN
        case _                         => ARETURN
      })

      ch.freeze
    }

    def generateMainMethodCode(ch: CodeHandler, stmts: List[StatTree], cname: String): Unit = {
      varMap = new HashMap[String, Int]
      val codeCompiler = new CodeCompiler(ch, cname);
      stmts.foreach(codeCompiler.compile)
      ch << RETURN
      ch.freeze
    }

    val outDir = ctx.outDir.map(_.getPath + "/").getOrElse("./")

    val f = new java.io.File(outDir)
    if (!f.exists()) {
      f.mkdir()
    }

    val sourceName = ctx.file.getName

    // output code
    prog.classes foreach {
      ct => generateClassFile(sourceName, ct, outDir)
    }

    // Now do the main method
    // ...
    val mainClassFile = new ClassFile(prog.main.id.value, None)
    mainClassFile.setSourceFile(sourceName)
    mainClassFile.addDefaultConstructor
    generateMainMethodCode(mainClassFile.addMainMethod.codeHandler, prog.main.stats, prog.main.id.value)
    mainClassFile.writeToFile(outDir + prog.main.id.value + ".class")
  }

}
