package tcompiler.code

import tcompiler.Main
import tcompiler.analyzer.Symbols._
import tcompiler.analyzer.Types._
import tcompiler.ast.Trees
import tcompiler.ast.Trees._
import tcompiler.imports.ImportMap
import tcompiler.utils.{Context, Pipeline}

import scala.collection.mutable.ListBuffer

/**
  * Created by Tim Lindeberg on 7/1/2016.
  */
object Desugaring extends Pipeline[List[CompilationUnit], List[CompilationUnit]] {

  override def run(ctx: Context)(cus: List[CompilationUnit]): List[CompilationUnit] = cus map desugar

  def desugar(cu: CompilationUnit) = {
    val desugarer = new Desugarer(cu.importMap)
    desugarer(cu)
  }

}

class Desugarer(importMap: ImportMap) extends Trees.Transformer {

  private val ThisName = "$this"

  override protected def _transform(t: Tree) = t match {
    case extensionDecl: ExtensionDecl                    => desugarExtensionDecl(super._transform(extensionDecl))
    case slice: ArraySlice                               => desugarArraySlice(super._transform(slice))
    case opDecl: OperatorDecl                            => replaceOperatorDecl(super._transform(opDecl))
    case safeAccess: SafeAccess                          =>
      // Recurse again to replace all calls in the desugared version
      super._transform(desugarSafeAccess(safeAccess))
    case acc@NormalAccess(_, MethodCall(meth, _)) =>
      val newAcc = super._transform(acc)
      val classSymbol = meth.getSymbol.classSymbol
      classSymbol match {
        case e: ExtensionClassSymbol => replaceExtensionCall(newAcc)
        case _                       => newAcc
      }
    case incDec: IncrementDecrementTree                  =>
      if (isObject(incDec.expr))
        replaceOperatorCall(super._transform(incDec))
      else
        desugarIncrementDecrement(super._transform(incDec))
    case assign: Assign                                  =>
      val to = assign.to
      if (to.isInstanceOf[ArrayRead] && isObject(to)) {
        val expr = super.apply(assign.from)
        val newAssign = treeCopy.Assign(assign, to, expr)
        // Transform again to replace external method calls etc.
        _transform(replaceOperatorCall(newAssign))
      } else {
        super._transform(assign)
      }
    case op: OperatorTree                                =>
      replaceOperatorCall(super._transform(op)) match {
        case op: OperatorTree => op // Finished transform
        case tree             => _transform(tree) // Transform again to replace external method calls etc.
      }
    case foreach: Foreach                                => _transform(desugarForeach(foreach))
    case elvis: Elvis                                    => _transform(desugarElvisOp(elvis))
    case _                                               => super._transform(t)
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
    * class $EX.A {
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
    val classSymbol = new ClassSymbol(exName, false)
    val originalClass = extensionClassSymbol.originalClassSymbol.get

    def replaceThis(stat: StatTree, thisId: VariableID) = {
      val transformThis = new Trees.Transformer {
        override protected def _transform(t: Tree) = t match {
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
        val thisSym = new VariableSymbol(ThisName).setType(TObject(originalClass))
        val thisId = VariableID(ThisName).setSymbol(thisSym)
        val newMethSym = new MethodSymbol(methSym.name, classSymbol, None, modifiers).setType(methSym)
        newMethSym.argList = thisSym :: methSym.argList
        newMethSym.args = methSym.args + (ThisName -> thisSym)
        newMethSym.annotations = Main.TExtensionAnnotation :: methSym.annotations
        val thisArg = Formal(extensionDecl.id, thisId)

        // Replace references to this with the this variable
        val newStat = meth.stat map { s => replaceThis(s, thisId) }
        MethodDecl(meth.retType, meth.id, thisArg :: meth.args, newStat, modifiers).setSymbol(newMethSym)
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
      val classSym = new ClassSymbol(className, false)
      val classId = ClassID(className).setSymbol(classSym)

      if (methSym.isStatic) {
        treeCopy.NormalAccess(t, classId, method)
      } else {
        val modifiers = methSym.modifiers + Static()
        val newMethSymbol = new MethodSymbol(methSym.name, classSym, None, modifiers).setType(methSym)
        val originalClass = extSymbol.originalClassSymbol.get
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

        val opSymbol = op.lookupOperator((lhs.getType, rhs.getType), importMap).get
        val obj = getClassID(opSymbol)
        c.createMethodCall(obj, opSymbol, lhs, rhs)
      case UnaryOperatorTree(expr)      =>
        if (!isObject(expr))
          return op

        val opSymbol = op.lookupOperator(expr.getType, importMap).get
        val obj = getClassID(opSymbol)
        c.createMethodCall(obj, opSymbol, expr)
      case ArrayOperatorTree(arr)       =>
        if (!isObject(arr))
          return op

        val arrClassSymbol = arr.getType.asInstanceOf[TObject].classSymbol

        val (obj, args) = op match {
          case ArrayRead(obj, index) => (obj, List(index))
          case Assign(ArrayRead(obj, index), expr) => (obj, List(index, expr))
          case _ => ???
        }
        val opSymbol = arrClassSymbol.lookupOperator(op, args.map(_.getType), importMap).get
        c.createMethodCall(obj, opSymbol, args)

      case _                            => op
    }
  }

  private def replaceOperatorDecl(t: Tree): Tree = {
    if (!t.isInstanceOf[OperatorDecl])
      return t
    val op = t.asInstanceOf[OperatorDecl]
    val opSymbol = op.getSymbol.asInstanceOf[OperatorSymbol]

    val methodID = new MethodID(opSymbol.name).setSymbol(opSymbol).setPos(op)
    val methDecl =
      if (op.isAbstract)
        new MethodDecl(op.retType, methodID, op.args, op.stat, op.modifiers)
      else opSymbol.operatorType match {
        case Assign(ArrayRead(_, _), _) =>
          // Convert array assignment so the value is returned in order to be consistent with other
          // types of assignments
          val valueId = op.args(1).id
          val retType = new TreeBuilder().getTypeTree(valueId.getType)
          val ret = Return(Some(valueId)).setType(valueId.getType)
          val stats: List[StatTree] = op.stat.get match {
            case Block(stats)        =>
              val last = stats.last match {
                case Return(Some(s)) => s
                case s: StatTree     => s
              }

              stats.drop(1) :+ last :+ ret
            case Return(Some(s)) => List(s, ret)
            case stat: StatTree  => List(stat, ret)
          }
          opSymbol.setType(valueId.getType)
          new MethodDecl(Some(retType), methodID, op.args, Some(Block(stats)), op.modifiers)
        case _                          =>
          new MethodDecl(op.retType, methodID, op.args, op.stat, op.modifiers)
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
      case TArray(arrTpe)       => desugarArrayForeach(varDecl, container, stat)
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
    val valInit = varDecl.copy(init = init).setPos(stat)
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

    val iteratorCall = c.createMethodCall(container, classSymbol, "Iterator", importMap)
    val iterator = c.putVarDecl("it", iteratorCall)

    val iteratorClass = iteratorCall.getType.asInstanceOf[TObject].classSymbol

    val comparisonCall = c.createMethodCall(iterator, iteratorClass, "HasNext", importMap)
    val nextMethodCall = c.createMethodCall(iterator, iteratorClass, "Next", importMap)

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
    * val a = <arr>[<start>:<end>]
    *
    * becomes:
    *
    * var $container = <arr>
    * var $start = < 0|slice.start >            // 0 if slice.start is undefined
    * var $end = < container.Size()|slice.end > // container.Size() if slice.end is undefined
    * var $slice = new <arrTpe>[$start - $end]
    * for(var $i = $start; i < $end; i++)
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

    val c = new TreeBuilder

    val arr = arraySlice.arr
    val arrayType = arr.getType
    val arrType = arrayType.asInstanceOf[TArray].tpe
    val container = c.putVarDecl("container", arr)

    val sizeCall = c.createMethodCall(container, "Size", Int)

    val start = c.putVarDecl("start", arraySlice.start.getOrElse(IntLit(0)))
    val end = c.putVarDecl("end", arraySlice.end.getOrElse(sizeCall).setType(Int))

    val size = List(Minus(end, start).setType(Int))
    val typeTree = c.getTypeTree(arrayType)
    val newArray = NewArray(typeTree, size).setType(arr)
    val slice = c.putVarDecl("slice", newArray)

    val indexDecl = c.createVarDecl("i", start)
    val indexId = indexDecl.id
    val comparison = LessThan(indexId, end).setType(Bool)
    val post = Assign(indexId, Plus(indexId, IntLit(1)).setType(Int)).setType(Int)


    val toSlice = ArrayRead(slice, Minus(indexId, start).setType(Int)).setType(arraySlice)
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

  private def isObject(e: ExprTree) = e.getType.isInstanceOf[TObject]

  private def getClassID(operatorSymbol: OperatorSymbol) = {
    val classSymbol = operatorSymbol.classSymbol
    new ClassID(classSymbol.name).setSymbol(classSymbol)
  }
}


