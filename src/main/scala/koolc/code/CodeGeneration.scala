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

    /** Writes the proper .class file in a given directory. An empty string for dir is equivalent to "./". */
    def generateClassFile(sourceName: String, ct: ClassDecl, dir: String): Unit = {
      // TODO: Create code handler, save to files ...
      val sym = ct.getSymbol
      val classFile = new ClassFile(sym.name, sym.parent.map(_.name))
      classFile.setSourceFile(sourceName)

      sym.members.foreach {
        case (name, varSymbol) =>
          classFile.addField(varSymbol.getType.byteCodeName + ";", name)
      }

      ct.methods.foreach { mt =>
        val methSymbol = mt.getSymbol
        val argType = methSymbol.argList.map(_.getType.byteCodeName).mkString + ";"
        val mh = classFile.addMethod(methSymbol.getType.byteCodeName + ";", methSymbol.name, argType)
        generateMethodCode(mh.codeHandler, mt)
      }

      classFile.writeToFile(if (dir.length == 0) "./" else dir + sourceName + ".class")
    }

    // a mapping from variable symbols to positions in the local variables
    // of the stack frame
    def generateMethodCode(ch: CodeHandler, mt: MethodDecl): Unit = {
      val methSym = mt.getSymbol
      val varMap = new HashMap[String, Int]
      val argMap = new HashMap[String, Int]

      mt.args.zipWithIndex.foreach { case (arg, i) => argMap(arg.getSymbol.name) = i + 1 }
      mt.vars foreach { variable => varMap(variable.getSymbol.name) = ch.getFreshVar }

      def load(id: Identifier): AbstractByteCodeGenerator = {
        val name = id.value
        val tp = id.getType

        if (varMap.contains(name)) tp match {
          case TBool | TInt => ILoad(varMap(name))
          case _            => ALoad(varMap(name))
        }
        else if (argMap.contains(name))
          ArgLoad(argMap(name))
        else
          GetField(mt.getSymbol.classSymbol.name, name, tp.byteCodeName)
      }

      def compileStat(stat: StatTree): Unit = stat match {
        case Block(stats) => stats.foreach(compileStat)
        case If(expr, thn, els) =>
          val thnLabel = ch.getFreshLabel("then")
          val elsLabel = ch.getFreshLabel("else")

          branch(expr, Label(thnLabel), Label(elsLabel))
          compileStat(thn)
          els match {
            case Some(elseSt) =>
              val afterLabel = ch.getFreshLabel("after")
              ch << Goto(afterLabel)
              ch << Label(elsLabel)
              compileStat(elseSt)
              ch << Label(afterLabel)
            case None => ch << Label(elsLabel)
          }

        case While(expr, stat) =>
          val beforeLabel = ch.getFreshLabel("before")
          val bodyLabel = ch.getFreshLabel("body")
          val afterLabel = ch.getFreshLabel("after")
          ch << Label(beforeLabel)
          branch(expr, Label(bodyLabel), Label(afterLabel))
          ch << Label(bodyLabel)
          compileStat(stat)
          ch << Goto(beforeLabel)
          ch << Label(afterLabel)
        case Println(expr)                => ???
        case Assign(id, expr)             => ???
        case ArrayAssign(id, index, expr) => ???
      }

      def branch(expr: ExprTree, thn: Label, els: Label): Unit = expr match {
        case Not(expr) => branch(expr, els, thn)
        case And(lhs, rhs) =>
          val next = Label(ch.getFreshLabel("next"))
          branch(lhs, next, els)
          ch << next
          branch(rhs, thn, els)
        case Or(lhs, rhs) =>
          val next = Label(ch.getFreshLabel("next"))
          branch(lhs, thn, next)
          ch << next
          branch(rhs, thn, els)
        case True() =>
          ch << Goto(thn.id)
        case False() =>
          ch << Goto(els.id)
        case id @ Identifier(value) =>
          ch << load(id)
          ch << IfEq(els.id)
          ch << Goto(thn.id)
        case LessThan(lhs, rhs) =>
          compileExpr(lhs)
          compileExpr(rhs)
          ch << If_ICmpLe(thn.id)
          ch << Goto(els.id)
        case Equals(lhs, rhs) =>
          compileExpr(lhs)
          compileExpr(rhs)
          lhs.getType match {
            case TInt | TBool => ch << If_ICmpEq(thn.id)
            case _            => ch << If_ACmpEq(thn.id)
          }
          ch << Goto(els.id)
      }

      def compileExpr(expr: ExprTree): Unit = expr match {
        case And(_, _) | Or(_, _) | Equals(_, _) | LessThan(_, _) | Not(_) =>
          val thn = ch.getFreshLabel("then")
          val els = ch.getFreshLabel("else")
          val after = ch.getFreshLabel("after")
          branch(expr, Label(thn), Label(els))
          ch << Label(thn)
          ch << Ldc(1)
          ch << Goto(after)
          ch << Label(els)
          ch << Ldc(0)
          ch << Label(after)
        case True()  => ch << Ldc(1)
        case False() => ch << Ldc(0)
        case Plus(lhs, rhs) =>
          compileExpr(lhs)
          compileExpr(rhs)
          ch << IADD
        case Minus(lhs, rhs) =>
          compileExpr(lhs)
          compileExpr(rhs)
          ch << ISUB
        case Times(lhs, rhs) =>
          compileExpr(lhs)
          compileExpr(rhs)
          ch << IMUL
        case Div(lhs, rhs) =>
          compileExpr(lhs)
          compileExpr(rhs)
          ch << IDIV
        case ArrayRead(arr, index) =>
          compileExpr(arr)
          compileExpr(index)
          arr.getType match {
            case TInt | TBool => ch << IALOAD
            case _            => ch << AALOAD
          }
        case ArrayLength(arr) =>
          compileExpr(arr)
          ch << ARRAYLENGTH
        case MethodCall(obj, meth, args) =>
          compileExpr(obj)
          args.foreach(compileExpr)
          val argType = args.map(_.getType.byteCodeName).mkString + ";"
          val name = obj.getType.asInstanceOf[TObject].classSymbol.name
          ch << InvokeVirtual(name, meth.value, argType)
        case IntLit(value) =>
          ch << Ldc(value)
        case StringLit(value) =>
          ch << Ldc(value)
        case id @ Identifier(value) =>
          ch << load(id)
        case This() =>
          ch << ArgLoad(0)
        case NewIntArray(size) =>
          compileExpr(size)
          ch << NewArray(10)
        case New(tpe) =>
          ch << DefaultNew(tpe.value)
      }

      mt.stats.foreach(compile)
      ch.freeze
    }

    def generateMainMethodCode(ch: CodeHandler, stmts: List[StatTree], cname: String): Unit = {

      // TODO: Emit code
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

  }

}
