package tlang.compiler.code

import tlang.compiler.analyzer.Symbols._
import tlang.compiler.analyzer.Types._
import tlang.compiler.ast.Trees
import tlang.compiler.ast.Trees._
import tlang.compiler.imports.Imports
import tlang.compiler.{CompilerPhase, DebugOutputFormatter}
import tlang.utils.Extensions._
import tlang.utils.formatting.Formatting
import tlang.{Constants, Context}

import scala.collection.mutable.ListBuffer

object Lowering extends CompilerPhase[CompilationUnit, CompilationUnit] {

  override def run(ctx: Context)(cus: List[CompilationUnit]): List[CompilationUnit] = cus map { cu =>
    val lowerer = new Lowerer(cu.imports)
    lowerer(cu)
  }

  override def description(formatting: Formatting): String =
    "Lowers the tree to simpler components. Performs desugaring."

  override def printDebugOutput(output: List[CompilationUnit], debugOutputFormatter: DebugOutputFormatter): Unit =
    debugOutputFormatter.printASTs(phaseName, output)

}

class Lowerer(imports: Imports) {

  private val ThisName = "$this"

  def apply(cu: CompilationUnit): CompilationUnit = {
    val first = firstPass(cu)
    val second = secondPass(first)
    second
  }

  private def firstPass(cu: CompilationUnit): CompilationUnit = {
    val transformer = new Trees.Transformer {

      override protected def _transform(t: Tree): Tree = {
        t match {
          case opDecl: OperatorDecl         =>
            replaceOperatorDecl(opDecl)
          case extensionDecl: ExtensionDecl =>
            desugarExtensionDecl(super._transform(extensionDecl))
          case methodDecl: MethodDeclTree   => methodDecl // stop here for now, no need to recurse in to stats etc.
          case _                            => super._transform(t)
        }
      }

    }
    transformer(cu)
  }

  private def secondPass(cu: CompilationUnit): CompilationUnit = {
    val transformer = new Trees.Transformer {

      override protected def _transform(t: Tree): Tree = t match {
        case slice: ArraySlice if !isObject(slice)                          =>
          super._transform(desugarArraySlice(super._transform(slice)))
        case incDec: IncrementDecrementTree if incDec.getType in Primitives =>
          super._transform(desugarIncrementDecrement(incDec))
        case safeAccess: SafeAccess                                         =>
          // Recurse again to replace all calls in the desugared version
          super._transform(desugarSafeAccess(safeAccess))
        case acc@NormalAccess(_, MethodCall(meth, _))                       =>
          val newAcc = super._transform(acc)
          val classSymbol = meth.getSymbol.classSymbol
          classSymbol match {
            case _: ExtensionClassSymbol => replaceExtensionCall(newAcc)
            case _                       => newAcc
          }
        case assign: Assign                                                 =>
          val to = assign.to
          to match {
            case ArrayRead(arr, _) if arr.getType.isInstanceOf[TObject] =>
              val expr = super.apply(assign.from)
              val newAssign = treeCopy.Assign(assign, to, expr)
              // Transform again to replace external method calls etc.
              _transform(replaceOperatorCall(newAssign))
            case _                                                      => super._transform(assign)
          }
        case op: OperatorTree                                               =>
          replaceOperatorCall(super._transform(op)) match {
            case op: OperatorTree => op // Finished transform
            case tree             => _transform(tree) // Transform again to replace external method calls etc.
          }
        case foreach: Foreach                                               => _transform(desugarForeach(foreach))
        case elvis: Elvis                                                   => _transform(desugarElvisOp(elvis))
        case _                                                              => super._transform(t)
      }
    }
    transformer(cu)
  }

  //@formatter:off
  /**
    * Replaces an extensions class with a class with all static methods.
    *
    * Examples:
    * --------------------------------------------------------------------------------
    *
    * extension A {
    *
    *   Def Method(arg1: A, arg2: Int) = {
    *     OtherMethod() + arg1.i + i + arg2.M()
    *   }
    *
    *   Def static StaticMethod(arg: Int) = arg + 1
    *
    * }
    *
    * becomes:
    *
    * class $EX::A {
    *
    *   Def static Method($this: A, arg1: A, arg2: Int) = {
    *     $this.OtherMethod() + arg1.i + $this.i + arg2.M()
    *   }
    *
    *   Def static StaticMethod(arg: Int) = arg + 1
    *
    * }
    *
    * --------------------------------------------------------------------------------
    */
  //@formatter:on
  private def desugarExtensionDecl(t: Tree): Tree = {
    if (!t.isInstanceOf[ExtensionDecl])
      return t

    val extensionDecl = t.asInstanceOf[ExtensionDecl]
    val extensionClassSymbol = extensionDecl.getSymbol.asInstanceOf[ExtensionClassSymbol]
    val exName = extensionClassSymbol.name
    val classSymbol = new ClassSymbol(exName)

    def replaceThis(stat: StatTree, thisId: VariableID) = {
      val transformThis = new Trees.Transformer {
        override protected def _transform(t: Tree): Tree = t match {
          case This()                                                      =>
            thisId
          case Access(obj, app)                                            =>
            // Don't replace VariableIDs in app since that would replace e.g A.i with A.$this.i
            val a = app match {
              case _: VariableID => app
              case _             => apply(app)
            }
            treeCopy.NormalAccess(t, apply(obj), a)
          case v@VariableID(name) if v.getSymbol.isInstanceOf[FieldSymbol] =>
            NormalAccess(thisId, v).setType(v)
          case _                                                           =>
            super._transform(t)
        }
      }
      transformThis(stat)
    }

    val newMethods = extensionDecl.methods.map { meth =>
      if (meth.isStatic) {
        // No need to transform static methods
        meth
      } else {
        val methSym = meth.getSymbol
        val modifiers = meth.modifiers + Static()
        val extendedType = extensionClassSymbol.getExtendedType
        val thisSym = new VariableSymbol(ThisName).setType(extendedType)
        val thisId = VariableID(ThisName).setSymbol(thisSym)
        val newMethSym = new MethodSymbol(methSym.name, classSymbol, None, modifiers).setType(methSym)
        newMethSym.argList = thisSym :: methSym.argList
        newMethSym.args = methSym.args + (ThisName -> thisSym)
        newMethSym.annotations = Constants.ExtensionAnnotation :: methSym.annotations
        val thisArg = Formal(extensionDecl.tpe, thisId)

        // Replace references to this with the this variable
        val newStat = meth.stat map { s => replaceThis(s, thisId) }
        MethodDecl(modifiers, meth.id, thisArg :: meth.args, meth.retType, newStat).setSymbol(newMethSym)
      }
    }

    val classId = ClassID(exName).setSymbol(classSymbol)
    ClassDecl(classId, Nil, Nil, newMethods).setSymbol(classSymbol).setPos(extensionDecl)
  }


  //@formatter:off
  /**
    * Replaces a call to an extension method with a static call.
    *
    * Examples:
    * --------------------------------------------------------------------------------
    *
    * a.ExtensionMethod(1, 2, 3)
    *
    * Becomes:
    *
    * $EX::A.ExtensionMethod(a, 1, 2, 3)
    *
    * --------------------------------------------------------------------------------
    */
  //@formatter:on
  private def replaceExtensionCall(t: Tree): Tree = t match {
    case Access(obj, method@MethodCall(meth, args)) =>
      val methSym = meth.getSymbol
      val extSymbol = meth.getSymbol.classSymbol.asInstanceOf[ExtensionClassSymbol]
      val className = extSymbol.name
      val classSym = new ClassSymbol(className)
      val classId = ClassID(className).setSymbol(classSym)
      val treeCopy = new Trees.LazyCopier
      if (methSym.isStatic) {
        treeCopy.NormalAccess(t, classId, method)
      } else {
        val modifiers = methSym.modifiers + Static()
        val newMethSymbol = new MethodSymbol(methSym.name, classSym, None, modifiers).setType(methSym)
        val originalClass = extSymbol.extendedType.get
        val newArg = new VariableSymbol(ThisName).setType(originalClass)
        newMethSymbol.argList = newArg :: methSym.argList
        val methId = MethodID(meth.name).setSymbol(newMethSymbol)
        val newMethCall = treeCopy.MethodCall(method, methId, obj :: args)
        treeCopy.NormalAccess(t, classId, newMethCall)
      }
    case _                                          => t
  }


  //@formatter:off
  /**
    * Replaces overloaded operator calls with calls to static methods
    *
    * a + b => A.$Plus(a, b)
    *
    * or
    *
    * ++a => A.$PreIncrement(a)
    *
    * or
    *
    * a[5] = 5 => a.$ArrayAssign(5, 5)
    */
  //@formatter:on
  private def replaceOperatorCall(t: Tree): Tree = {
    if (!t.isInstanceOf[OperatorTree])
      return t

    val op = t.asInstanceOf[OperatorTree]
    op match {
      // Don't replace these operators
      case _: And | _: Or | _: Not | _: ExtractNullable => return op
      case _                                            =>
    }


    val c = new TreeBuilder

    op match {
      case BinaryOperatorTree(lhs, rhs) =>
        // Don't replace null checks
        if (lhs.isInstanceOf[NullLit] || rhs.isInstanceOf[NullLit])
          return op

        if (!(isObject(lhs) || isObject(rhs)))
          return op

        val opSymbol = op.lookupOperator((lhs.getType, rhs.getType), imports).get
        val obj = getClassID(opSymbol)
        c.createMethodCall(obj, opSymbol, lhs, rhs)
      case UnaryOperatorTree(expr)      =>
        if (!isObject(expr))
          return op

        val opSymbol = op.lookupOperator(expr.getType, imports).get
        val obj = getClassID(opSymbol)
        c.createMethodCall(obj, opSymbol, expr)
      case ArrayOperatorTree(arr)       =>
        if (!isObject(arr))
          return op

        val arrClassSymbol = arr.getType.asInstanceOf[TObject].classSymbol

        val (obj, args) = op match {
          case ArrayRead(obj, index)               =>
            (obj, List(index))
          case Assign(ArrayRead(obj, index), expr) =>
            (obj, List(index, expr))
          case ArraySlice(obj, start, end, step)   =>
            val s = start.getOrElse(NullLit()).setType(Int.getNullable)
            val e = end.getOrElse(NullLit()).setType(Int.getNullable)
            val st = step.getOrElse(NullLit()).setType(Int.getNullable)
            (obj, List(s, e, st))
          case _                                   => ???
        }
        val opSymbol = arrClassSymbol.lookupOperator(op, args.map(_.getType), imports).get
        if (op.isInstanceOf[Assign])
          opSymbol.setType(args(1))
        c.createMethodCall(obj, opSymbol, args)
      case _                            => op
    }
  }

  private def replaceOperatorDecl(t: Tree): Tree = {
    if (!t.isInstanceOf[OperatorDecl])
      return t

    val op = t.asInstanceOf[OperatorDecl]
    val opSymbol = op.getSymbol.asInstanceOf[OperatorSymbol]

    val methodID = MethodID(opSymbol.name).setSymbol(opSymbol).setPos(op)
    val methDecl =
      if (op.isAbstract)
        MethodDecl(op.modifiers, methodID, op.args, op.retType, op.stat)
      else opSymbol.operatorType match {
        case Assign(ArrayRead(_, _), _) =>
          // Convert array assignment so the value is returned in order to be consistent with other
          // types of assignments
          val valueId = op.args(1).id
          val retType = new TreeBuilder().getTypeTree(valueId.getType)
          val ret = Return(Some(valueId)).setType(valueId.getType)
          val stats: List[StatTree] = op.stat.get match {
            case Block(stats)    =>
              val last = stats.last match {
                case Return(Some(s)) => s
                case s: StatTree     => s
              }

              stats.drop(1) :+ last :+ ret
            case Return(Some(s)) => List(s, ret)
            case stat: StatTree  => List(stat, ret)
          }
          opSymbol.setType(valueId.getType)
          MethodDecl(op.modifiers, methodID, op.args, Some(retType), Some(Block(stats)))
        case _                          =>
          MethodDecl(op.modifiers, methodID, op.args, op.retType, op.stat)
      }
    methDecl.setSymbol(opSymbol).setPos(op)
  }

  //@formatter:off
  /**
    * Transform increment and decrement expressions on accesses and array reads.
    *
    * Examples:
    * --------------------------------------------------------------------------------
    *
    * a++
    *
    * becomes:
    *
    * var $v = a
    * a = a + 1
    * $v
    *
    * --------------------------------------------------------------------------------
    *
    * --a
    *
    * becomes:
    *
    * a = a - 1
    * a
    *
    * --------------------------------------------------------------------------------
    *
    * GetObject().I++
    *
    * becomes:
    *
    * var $obj = GetObject()
    * var $v = $tmp.I
    * var $newV = $v + 1
    * $tmp.I = $newV
    * $v
    *
    * --------------------------------------------------------------------------------
    *
    * --GetArray()[GetIndex()*4]
    *
    * becomes:
    *
    * var $arr
    * var $idx = GetIndex()*4
    * var $v = a[$idx]
    * var $newV = $v - 1
    * $arr[$idx] = $newV
    * newV$x
    *
    * --------------------------------------------------------------------------------
    */
  //@formatter:on
  private def desugarIncrementDecrement(t: Tree): Tree = {
    if (!t.isInstanceOf[IncrementDecrementTree])
      return t

    val incDec = t.asInstanceOf[IncrementDecrementTree]
    val c = new TreeBuilder

    def getPlusOrMinus(value: ExprTree) = {
      val o = c.createOne(value.getType)
      val plusOrMinus = if (incDec.isIncrement) Plus(value, o) else Minus(value, o)
      plusOrMinus.setType(value)
    }

    def putResult(to: Assignable, from: ExprTree, value: ExprTree) = {
      c.put(Assign(to, from).setType(to))
      c.put(PutValue(value))
      c.setPos(incDec)
      c.getCode
    }

    // Simple case first:
    incDec.expr match {
      case variable: VariableID =>
        val plusOrMinus = getPlusOrMinus(variable)
        val v = if (incDec.isPre) variable
        else c.putVarDecl("v", variable)

        return putResult(variable, plusOrMinus, v)
      case _                    =>
    }

    // Otherwise we have an access or array read
    val (assignTo, value) = incDec.expr match {
      case acc@Access(obj, application)  =>
        obj match {
          case _: Identifier[_] =>
            val v = if (incDec.isPre) acc else c.putVarDecl("v", acc)
            (acc, v)
          case _                =>
            val objId = c.putVarDecl("obj", obj)
            val newAccess = NormalAccess(objId, application).setType(application)
            val v = if (newAccess.isStatic && incDec.isPre) newAccess else c.putVarDecl("v", newAccess)
            (newAccess, v)
        }
      case arrRead@ArrayRead(arr, index) =>
        val arrId = arr match {
          case _: VariableID => arr
          case _             => c.putVarDecl("arr", arr)
        }
        val indexId = index match {
          case _: VariableID | _: Literal[_] => index
          case _                             => c.putVarDecl("idx", index)
        }

        val a = ArrayRead(arrId, indexId).setType(arrRead)
        (a, c.putVarDecl("v", a))
    }

    val plusOrMinus = getPlusOrMinus(value)
    if (incDec.isPre) {
      val newValue = c.putVarDecl("newV", plusOrMinus)
      putResult(assignTo, newValue, newValue)
    } else {
      putResult(assignTo, plusOrMinus, value)
    }
  }


  /**
    * Desugars for each loops, either array based or an iterator based.
    */
  private def desugarForeach(t: Tree): Tree = {
    if (!t.isInstanceOf[Foreach])
      return t

    val foreach = t.asInstanceOf[Foreach]
    val container = foreach.container
    val varDecl = foreach.varDecl
    val stat = foreach.stat
    container.getType match {
      case _: TArray            => desugarArrayForeach(varDecl, container, stat)
      case TObject(classSymbol) => desugarIteratorForeach(classSymbol, varDecl, container, stat)
      case _                    => ???
    }
  }


  //@formatter:off
  /**
    * Transforms foreach loop over an array
    *
    * Examples:
    *
    * --------------------------------------------------------------------------------
    *
    * for(<varDecl> in <array>)
    *   <code>
    *
    * becomes:
    *
    * val $container = <array>
    * for(var $i = 0; $i < $container.Size(); i++){
    *   <varDecl> = $container[$i]
    *   <code>
    * }
    *
    * --------------------------------------------------------------------------------
    */
  //@formatter:on
  private def desugarArrayForeach(varDecl: VarDecl, container: ExprTree, stat: StatTree) = {
    val c = new TreeBuilder
    val indexDecl = c.createVarDecl("i", IntLit(0))
    val index = indexDecl.id

    val containerId = c.putVarDecl("container", container)

    val sizeCall = c.createMethodCall(containerId, "Size", Int)

    val comparison = LessThan(index, sizeCall).setType(Bool).setPos(varDecl)
    val post = Assign(index, Plus(index, IntLit(1)).setType(Int)).setType(Int).setPos(varDecl)

    val arrReadType = containerId.getType.asInstanceOf[TArray].tpe
    val init = Some(ArrayRead(containerId, index).setType(arrReadType).setPos(varDecl))
    val valInit = varDecl.copy(initation = init).setPos(stat)
    valInit.setSymbol(varDecl.getSymbol).setPos(varDecl)
    val stats = stat match {
      case Block(s) => Block(valInit :: s)
      case _        => Block(List(valInit, stat))
    }

    c.put(For(List(indexDecl), comparison, List(post), stats).setPos(stat))
    c.getCode
  }

  //@formatter:off
  /**
    * Transforms a foreach loop
    *
    * Examples:
    *
    * --------------------------------------------------------------------------------
    *
    * for(<varDecl> in <container>)
    *   <code>
    *
    * becomes:
    *
    * val $it = <container>.Iterator()
    * while($it.HasNext()) {
    *   <varDecl> = $it.Iterator()
    *   <code>
    * }
    *
    * --------------------------------------------------------------------------------
    */
  //@formatter:on
  private def desugarIteratorForeach(classSymbol: ClassSymbol, varDecl: VarDecl, container: ExprTree, stat: StatTree) = {
    val c = new TreeBuilder

    val iteratorCall = c.createMethodCall(container, classSymbol, "Iterator", imports, List())
    val iterator = c.putVarDecl("it", iteratorCall)

    val iteratorClass = iteratorCall.getType.asInstanceOf[TObject].classSymbol

    val comparisonCall = c.createMethodCall(iterator, iteratorClass, "HasNext", imports, List())
    val nextMethodCall = c.createMethodCall(iterator, iteratorClass, "Next", imports, List())

    val valInit = VarDecl(varDecl.tpe, varDecl.id, Some(nextMethodCall), varDecl.modifiers).setPos(stat)
    valInit.setSymbol(varDecl.getSymbol)
    val stats = stat match {
      case Block(s) => Block(valInit :: s)
      case _        => Block(List(valInit, stat))
    }

    c.put(While(comparisonCall, stats))
    c.getCode
  }

  //@formatter:off
  /**
    * Transforms an array slice
    *
    * Examples:
    *
    * --------------------------------------------------------------------------------
    *
    * val a = <arr>[<start>:<end>:<step>]
    *
    * becomes:
    *
    * var $container = <arr>
    * var $start     = <start> | 0
    * var $end       = <end>   | container.Size())
    * var $step      = <step>  | 1
    * var $slice = new <arrTpe>[(($start - $end) + $step - 1) / $step)]
    * for(var $i = $start; i < $end; i += $step)
    *   $slice[$start - $i] = $container[$i]
    * $slice
    *
    * --------------------------------------------------------------------------------
    */
  //@formatter:on
  private def desugarArraySlice(t: Tree): Tree = {
    if (!t.isInstanceOf[ArraySlice])
      return t

    val arraySlice = t.asInstanceOf[ArraySlice]

    val sliceType = arraySlice.arr.getType
    if (sliceType.isInstanceOf[TObject])
      return arraySlice


    val arr = arraySlice.arr
    val arrayType = arr.getType
    if (!arrayType.isInstanceOf[TArray])
      return t

    val arrType = arrayType.asInstanceOf[TArray].tpe

    val c = new TreeBuilder


    val container = c.putVarDecl("container", arr)

    val sizeCall = c.createMethodCall(container, "Size", Int)

    val start = c.putVarDecl("start", arraySlice.start.getOrElse(IntLit(0)))
    val end = c.putVarDecl("end", arraySlice.end.getOrElse(sizeCall).setType(Int))
    val step = c.putVarDecl("step", arraySlice.step.getOrElse(IntLit(1)))

    var size: ExprTree = Minus(end, start).setType(Int)
    if (arraySlice.step.isDefined)
      size = Div(Plus(size, Minus(step, IntLit(1)).setType(Int)).setType(Int), step).setType(Int)

    val typeTree = c.getTypeTree(arrayType)
    val newArray = NewArray(typeTree, List(size)).setType(arr)
    val slice = c.putVarDecl("slice", newArray)

    val indexDecl = c.createVarDecl("i", start)
    val indexId = indexDecl.id
    val comparison = LessThan(indexId, end).setType(Bool)
    val post = Assign(indexId, Plus(indexId, step).setType(Int)).setType(Int)

    val index = Div(Minus(indexId, start).setType(Int), step).setType(Int)
    val toSlice = ArrayRead(slice, index).setType(arraySlice)
    val fromArr = ArrayRead(container, indexId).setType(arrType)
    val copyValue = Assign(toSlice, fromArr).setType(arrType)

    c.put(For(List(indexDecl), comparison, List(post), copyValue))
    c.put(slice)

    c.setPos(arraySlice)

    c.getCode
  }

  //@formatter:off
  /**
    * Transforms safe access calls
    *
    * Examples:
    *
    * --------------------------------------------------------------------------------
    *
    * A?.GetB()
    *
    * becomes:
    *
    * A != null ? A.GetB() : null
    *
    * --------------------------------------------------------------------------------
    *
    * A?.GetB()?.GetC()?.GetD()?.E
    *
    * becomes:
    *
    * if(A != null) {
    *   val tmp$1 = A.GetB()
    *   if(tmp$1 != null) {
    *     val tmp$2 = tmp$1.GetC()
    *     if(tmp$2 != null) {
    *       val tmp$3 = tmp$2.GetD()
    *       tmp$3 != null ? tmp$3.E : null
    *     } else {
    *       null
    *   } else {
    *     null
    * } else {
    *   null
    * }
    *
    * --------------------------------------------------------------------------------
    */
  //@formatter:on
  private def desugarSafeAccess(t: Tree): Tree = {
    if (!t.isInstanceOf[SafeAccess])
      return t

    val c = new TreeBuilder
    var safeAccess = t.asInstanceOf[ExprTree]
    val apps = new ListBuffer[ExprTree]()

    while (safeAccess.isInstanceOf[SafeAccess]) {
      val s = safeAccess.asInstanceOf[SafeAccess]
      apps += s.application
      safeAccess = s.obj
    }

    val appsInOrder = apps.reverse.toList
    val obj = safeAccess

    def _desugar(i: Int, obj: ExprTree, apps: List[ExprTree]): StatTree = {
      val condition = NotEquals(obj, NullLit()).setType(Bool)
      val app = apps.head
      val access = NormalAccess(obj, app).setType(app.getType.getNonNullable)

      if (apps.size == 1) {
        val ternary = Ternary(condition, access, NullLit()).setType(app.getType.getNullable)
        return PutValue(ternary)
      }
      val ifNull = PutValue(NullLit())
      val varDecl = c.createVarDecl(s"tmp$i", access)
      val thn = List(varDecl, _desugar(i + 1, varDecl.id, apps.tail))
      If(condition, Block(thn), Some(Block(List(ifNull))))
    }

    c.put(_desugar(1, obj, appsInOrder))
    c.setPos(t)
    c.getCode.setType(t.asInstanceOf[SafeAccess])
  }

  //@formatter:off
  /**
    * Transforms the elvis operator
    *
    * Examples:
    *
    * --------------------------------------------------------------------------------
    *
    * val a = b ?: -1
    *
    * becomes:
    *
    * val a = b == null ? -1 : b
    *
    * --------------------------------------------------------------------------------
    */
  //@formatter:on
  private def desugarElvisOp(t: Tree): Tree = {
    if (!t.isInstanceOf[Elvis])
      return t

    val elvis = t.asInstanceOf[Elvis]
    val nullableValue = elvis.nullableValue
    val ifNull = elvis.ifNull

    val c = new TreeBuilder

    val nullableID = c.putVarDecl("tmp", nullableValue)

    val condition = Equals(nullableID, NullLit()).setType(Bool)
    c.put(Ternary(condition, ifNull, nullableID).setType(elvis))
    c.setPos(elvis)
    c.getCode
  }

  private def isObject(expr: ExprTree) = expr.getType.isInstanceOf[TObject]


  private def getClassID(operatorSymbol: OperatorSymbol) = {
    val classSymbol = operatorSymbol.classSymbol
    ClassID(classSymbol.name).setSymbol(classSymbol)
  }
}


