package koolc

import java.io.File
import koolc.lexer.Token
import scala.sys.process.ProcessLogger
import koolc.ast.Trees._
import koolc.analyzer.Symbols._
import koolc.analyzer.Types._

object TestUtils {
  val runScript = "./reference/run.sh"
  val resources = "./src/test/resources/"
  def programFiles(dir: String): Array[File] = {
    val f = new File(dir)
    if (f.exists()) {
      f.listFiles
    } else {
      f.mkdir
      Array[File]()
    }
  }
  def format(token: Token): String = token + "(" + token.line + ":" + token.col + ")"

  object IgnoreErrorOutput extends ProcessLogger {
    def buffer[T](f: ⇒ T): T = f
    def err(s: ⇒ String): Unit = {}
    def out(s: ⇒ String): Unit = {}
  }

  object HasTypes {

    def apply(prog: Program): Boolean = hasTypes(prog) && f(prog)

    def withoutMethodCalls(prog: Program): Boolean = hasTypes(prog)

    private def hasTypes(prog: Program) = {
      def flatten(l: List[_]): List[_] = l flatMap {
        case l1: List[_] => flatten(l1)
        case otherwise   => List(otherwise)
      }
      flatten(prog.classes.map(_.getSymbol).map(klass => {
        List(
          klass.getType,
          klass.members.map(_._2.getType),
          klass.methods.map(_._2.getType),
          klass.methods.map(_._2).flatMap(meth => {
            List(
              meth.argList.map(_.getType),
              meth.members.map(_._2.getType),
              meth.params.map(_._2.getType))
          }))
      })).forall(_ != TUntyped)
    }

    private def f(t: Tree): Boolean = {
      try {
        val s = t match {
          case Program(main, classes) =>
            f(main); trees(classes)
          case MainObject(id, stats) =>
            f(id); trees(stats)
          case ClassDecl(id, parent, vars, methods) =>
            f(id); optional(parent); trees(vars); trees(methods)
          case VarDecl(tpe, id) =>
            f(tpe); f(id)
          case MethodDecl(retType, id, args, vars, stats, retExpr) =>
            f(retType); f(id); trees(args); trees(vars); trees(stats); f(retExpr)
          case Formal(tpe, id) =>
            f(tpe); f(id)
          case Block(stats) => trees(stats)
          case If(expr, thn, els) =>
            f(expr); f(thn); optional(els)
          case While(expr, stat) =>
            f(expr); f(stat)
          case Println(expr) => f(expr)
          case Assign(id, expr) =>
            f(id); f(expr)
          case ArrayAssign(id, index, expr) =>
            f(id); f(index); f(expr)
          case And(lhs, rhs) =>
            f(lhs); f(rhs)
          case Or(lhs, rhs) =>
            f(lhs); f(rhs)
          case Plus(lhs, rhs) =>
            f(lhs); f(rhs)
          case Minus(lhs, rhs) =>
            f(lhs); f(rhs)
          case Times(lhs, rhs) =>
            f(lhs); f(rhs)
          case Div(lhs, rhs) =>
            f(lhs); f(rhs)
          case LessThan(lhs, rhs) =>
            f(lhs); f(rhs)
          case Equals(lhs, rhs) =>
            f(lhs); f(rhs)
          case ArrayRead(arr, index) =>
            f(arr); f(index)
          case ArrayLength(arr) => f(arr)
          case MethodCall(obj, meth, args) =>
            f(obj); f(meth); trees(args)
          case x @ IntLit(value)     => x.getType == TInt
          case x @ StringLit(value)  => x.getType == TString
          case x @ Identifier(value) => x.getType != TUntyped && x.getType != TError
          case NewIntArray(size)     => f(size)
          case New(tpe)              => f(tpe)
          case Not(expr)             => f(expr)
          case x @ IntType()         => x.getType == TInt
          case x @ IntArrayType()    => x.getType == TIntArray
          case x @ BooleanType()     => x.getType == TBool
          case x @ StringType()      => x.getType == TString
          case x @ True()            => x.getType == TBool
          case x @ False()           => x.getType == TBool
          case x @ This()            => x.getType != TUntyped && x.getType != TError
        }
        s
      } catch {
        case t: Throwable => false
      }
    }

    private def optional(t: Option[Tree]) = t match { case Some(p) => f(p) case None => true }
    private def trees(trees: List[Tree]) = trees.map(f).forall(_ == true)

  }
}

