package koolc
package code

import ast.Trees._
import analyzer.Symbols._
import analyzer.Types._
import cafebabe._
import AbstractByteCodes.{ New => _, _ }
import ByteCodes._
import utils._
import scala.collection.mutable.HashMap
import com.sun.org.apache.bcel.internal.generic.IF_ICMPLE

object CodeGeneration extends Pipeline[Program, Unit] {

  def run(ctx: Context)(prog: Program): Unit = {
    import ctx.reporter._

    var varMap = new HashMap[String, Int]

    def store(expr: ExprTree, ch: CodeHandler, id: Identifier, cn: String): Unit = {
      val name = id.value
      val tp = id.getType

      if (varMap.contains(name)) {
        compileExpr(expr, ch, cn)
        ch << (tp match {
          case TBool | TInt => IStore(varMap(name))
          case _            => AStore(varMap(name))
        })
      } else {
        ch << ArgLoad(0) // put this-reference on stack
        compileExpr(expr, ch, cn)
        ch << PutField(cn, name, tp.byteCodeName)
      }

    }

    def load(ch: CodeHandler, id: Identifier, cn: String): Unit = {
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

    def compileStat(stat: StatTree, ch: CodeHandler, cn: String): Unit = stat match {
      case Block(stats) => stats.foreach(compileStat(_, ch, cn))
      case If(expr, thn, els) =>
        val thnLabel = ch.getFreshLabel("then")
        val elsLabel = ch.getFreshLabel("else")
        val afterLabel = ch.getFreshLabel("after")

        branch(expr, Label(thnLabel), Label(elsLabel), ch, cn)
        ch << Label(thnLabel)
        compileStat(thn, ch, cn)
        ch << Goto(afterLabel)
        ch << Label(elsLabel)
        if(els.isDefined) compileStat(els.get, ch, cn)
        ch << Label(afterLabel)

      case While(expr, stat) =>
        val bodyLabel = ch.getFreshLabel("body")
        val afterLabel = ch.getFreshLabel("after")
        branch(expr, Label(bodyLabel), Label(afterLabel), ch, cn)
        ch << Label(bodyLabel)
        compileStat(stat, ch, cn)
        branch(expr, Label(bodyLabel), Label(afterLabel), ch, cn)
        ch << Label(afterLabel)
      case Println(expr) =>
        ch << GetStatic("java/lang/System", "out", "Ljava/io/PrintStream;")
        compileExpr(expr, ch, cn)
        ch << InvokeVirtual("java/io/PrintStream", "println", "(" + expr.getType.byteCodeName + ")V")
      case Assign(id, expr) =>
        store(expr, ch, id, cn)
      case ArrayAssign(id, index, expr) =>
        load(ch, id, cn)
        compileExpr(index, ch, cn)
        compileExpr(expr, ch, cn)
        ch << IASTORE
    }

    def branch(expr: ExprTree, thn: Label, els: Label, ch: CodeHandler, cn: String): Unit = expr match {
      case Not(expr) => branch(expr, els, thn, ch, cn)
      case And(lhs, rhs) =>
        val next = Label(ch.getFreshLabel("next"))
        branch(lhs, next, els, ch, cn)
        ch << next
        branch(rhs, thn, els, ch, cn)
      case Or(lhs, rhs) =>
        val next = Label(ch.getFreshLabel("next"))
        branch(lhs, thn, next, ch, cn)
        ch << next
        branch(rhs, thn, els, ch, cn)
      case True() =>
        ch << Goto(thn.id)
      case False() =>
        ch << Goto(els.id)
      case id @ Identifier(value) =>
        load(ch, id, cn)
        ch << IfEq(els.id)
        ch << Goto(thn.id)
      case LessThan(lhs, rhs) =>
        compileExpr(lhs, ch, cn)
        compileExpr(rhs, ch, cn)
        ch << If_ICmpLt(thn.id)
        ch << Goto(els.id)
      case Equals(lhs, rhs) =>
        compileExpr(lhs, ch, cn)
        compileExpr(rhs, ch, cn)
        lhs.getType match {
          case TInt | TBool => ch << If_ICmpEq(thn.id)
          case _            => ch << If_ACmpEq(thn.id)
        }
        ch << Goto(els.id)
      case mc @ MethodCall(obj, meth, args) =>
        compileExpr(mc, ch, cn)
        ch << IfEq(els.id)
        ch << Goto(thn.id)
    }

    def compileExpr(expr: ExprTree, ch: CodeHandler, cn: String): Unit = expr match {
      case And(_, _) | Or(_, _) | Equals(_, _) | LessThan(_, _) | Not(_) =>
        val thn = ch.getFreshLabel("then")
        val els = ch.getFreshLabel("else")
        val after = ch.getFreshLabel("after")
        branch(expr, Label(thn), Label(els), ch, cn)
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
            compileExpr(lhs, ch, cn)
            compileExpr(rhs, ch, cn)
            ch << IADD
          }
          case TString => {
            ch << DefaultNew("java/lang/StringBuilder")
            compileExpr(lhs, ch, cn)
            ch << InvokeVirtual("java/lang/StringBuilder", "append", "(" + lhs.getType.byteCodeName + ")Ljava/lang/StringBuilder;")
            compileExpr(rhs, ch, cn)
            ch << InvokeVirtual("java/lang/StringBuilder", "append", "(" + rhs.getType.byteCodeName + ")Ljava/lang/StringBuilder;")
            ch << InvokeVirtual("java/lang/StringBuilder", "toString", "()Ljava/lang/String;")
          }
          case _ => throw new UnsupportedOperationException(expr.toString)
        }

      case Minus(lhs, rhs) =>
        compileExpr(lhs, ch, cn)
        compileExpr(rhs, ch, cn)
        ch << ISUB
      case Times(lhs, rhs) =>
        compileExpr(lhs, ch, cn)
        compileExpr(rhs, ch, cn)
        ch << IMUL
      case Div(lhs, rhs) =>
        compileExpr(lhs, ch, cn)
        compileExpr(rhs, ch, cn)
        ch << IDIV
      case ArrayRead(arr, index) =>
        compileExpr(arr, ch, cn)
        compileExpr(index, ch, cn)
        ch << IALOAD
      case ArrayLength(arr) =>
        compileExpr(arr, ch, cn)
        ch << ARRAYLENGTH
      case mc @ MethodCall(obj, meth, args) =>
        compileExpr(obj, ch, cn)
        args.foreach(compileExpr(_, ch, cn))
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
        load(ch, id, cn)
      case This() =>
        ch << ArgLoad(0)
      case NewIntArray(size) =>
        compileExpr(size, ch, cn)
        ch << NewArray(10) // I?
      case New(tpe) =>
        ch << DefaultNew(tpe.value)
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
      mt.vars foreach { variable => varMap(variable.getSymbol.name) = ch.getFreshVar }

      mt.stats.foreach(compileStat(_, ch, mt.getSymbol.classSymbol.name))
      compileExpr(mt.retExpr, ch, methSym.classSymbol.name)

      ch << (mt.retType match {
        case IntType() | BooleanType() => IRETURN
        case _                         => ARETURN
      })

      ch.freeze
    }

    def generateMainMethodCode(ch: CodeHandler, stmts: List[StatTree], cname: String): Unit = {
      varMap = new HashMap[String, Int]
      stmts.foreach(compileStat(_, ch, cname))
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
