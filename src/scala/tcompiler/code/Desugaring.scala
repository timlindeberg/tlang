package tcompiler.code

import tcompiler.analyzer.Symbols.{ClassSymbol, MethodSymbol, VariableSymbol}
import tcompiler.analyzer.Types._
import tcompiler.ast.Trees._
import tcompiler.ast.{TreeTransformer, Trees}
import tcompiler.utils.{Context, Pipeline, Positioned}

import scala.collection.mutable.ListBuffer

/**
  * Created by Tim Lindeberg on 7/1/2016.
  */
object Desugaring extends Pipeline[List[CompilationUnit], List[CompilationUnit]] {

  override def run(ctx: Context)(cus: List[CompilationUnit]): List[CompilationUnit] = cus map desugar


  def desugar(cu: CompilationUnit) = {
    System.out.flush()
    val desugarer = new Desugarer()
    desugarer(cu)
  }
}
class Desugarer {

  def apply(cu: CompilationUnit) = {
    val desugarTransformer = new TreeTransformer {
      override def transform(t: Tree) = t match {
        case foreach: Foreach               => desugarForeachLoop(foreach)
        case slice: ArraySlice              => desugarArraySlice(slice)
        case incDec: IncrementDecrementTree => desugarIncrementDecrement(incDec)
        case _                              => super.transform(t)
      }

    }
    desugarTransformer.transform(cu).asInstanceOf[CompilationUnit]
  }


  /**
    * Desugars for each loops, either array based or an iterator based.
    */
  private def desugarForeachLoop(foreach: Foreach): StatTree = {
    val container = foreach.container
    val varDecl = foreach.varDecl
    val stat = foreach.stat
    container.getType match {
      case TArray(arrTpe)       => desugarArrayForeachLoop(varDecl, container, stat)
      case TObject(classSymbol) => desugarIteratorForeachLoop(classSymbol, varDecl, container, stat)
      case _                    => ???
    }
  }

  //@formatter:off
  /**
    * Transform increment and decrement expressions on accesses and array reads.
    *
    * Examples:
    *
    *   GetObject().I++
    *
    * in to:
    *
    *   var $obj = GetObject()
    *   var $v = $tmp.I
    *   var $newV = $v + 1
    *   $tmp.I = $newV
    *   $v
    *
    * or
    *
    *   --GetArray()[GetIndex()*4]
    *
    * in to:
    *
    *   var $arr
    *   var $idx = GetIndex()*4
    *   var $v = a[$idx]
    *   var $newV = $v - 1
    *   $arr[$idx] = $newV
    *   newV$x
    *
    */
    //@formatter:on
  private def desugarIncrementDecrement(incDec: IncrementDecrementTree): ExprTree = {
    // These get compiled to for instance IINC etc which is more effective
    // and can also be an overloaded operator if called on an object
    if (incDec.expr.isInstanceOf[VariableID])
      return incDec

    val c = new CodeBuilder
    val obj = incDec.expr match {
      case acc@Access(obj, application)  =>
        if (obj.isInstanceOf[ClassID]) {
          acc
        } else {
          val objId = c.putVarDecl("obj", obj)
          NormalAccess(objId, application).setType(application)
        }
      case arrRead@ArrayRead(arr, index) =>
        val arrId = c.putVarDecl("arr", arr)
        val indexId = c.putVarDecl("idx", index)
        ArrayRead(arrId, indexId).setType(arrRead)
    }

    val value = c.putVarDecl("v", obj)

    val o = c.createOne(value.getType)
    val plusOrMinus = if (incDec.isIncrement) Plus(value, o) else Minus(value, o)
    plusOrMinus.setType(value)

    val newValue = c.putVarDecl("newV", plusOrMinus)

    c.put(Assign(obj, newValue).setType(obj))
    c.put(IfDup(if (incDec.isPre) newValue else value))
    c.setPos(incDec)
    c.getCode
  }

  //@formatter:off
  /**
    * Transforms foreach loop over an array
    *
    *   for(<varDecl> in <array>)
    *     <code>
    *
    * in to:
    *
    *   val $container = <array>
    *   for(var $i = 0; $i < $container.Size(); i++){
    *     <varDecl> = $container[$i]
    *     <code>
    *   }
    *
    */
 //@formatter:on
  private def desugarArrayForeachLoop(varDecl: VarDecl, container: ExprTree, stat: StatTree) = {
    val c = new CodeBuilder
    val indexDecl = c.createVarDecl("i", IntLit(0).setType(TInt))
    val index = indexDecl.id

    val containerId = c.putVarDecl("container", container)

    val sizeCall = c.createMethodCall(container, "Size", TInt)

    val comparison = LessThan(index, sizeCall).setType(TBool).setPos(varDecl)
    val post = PostIncrement(index).setType(TInt).setPos(varDecl)

    val init = Some(ArrayRead(containerId, index).setType(containerId).setPos(varDecl))
    val valInit = varDecl.copy(init = init).setPos(stat)
    valInit.setSymbol(varDecl.getSymbol).setPos(varDecl)
    val stats = Block(List(valInit, stat))

    c.put(For(List(indexDecl), comparison, List(post), stats).setPos(stat))
    c.getCode
  }

  //@formatter:off
  /**
    * Transforms a foreach loop
    *
    *   for(<varDecl> in <container>)
    *     <code>
    *
    * in to:
    *
    *   val $it = <container>.Iterator()
    *   while($it.HasNext()){
    *     <varDecl> = $it.Iterator()
    *     <code>
    *   }
    */
  //@formatter:on
  private def desugarIteratorForeachLoop(classSymbol: ClassSymbol, varDecl: VarDecl, container: ExprTree, stat: StatTree) = {
    val c = new CodeBuilder

    val iteratorCall = c.createMethodCall(container, classSymbol, "Iterator")
    val iterator = c.putVarDecl("it", iteratorCall)

    val iteratorClass = iteratorCall.getType.asInstanceOf[TObject].classSymbol

    val comparisonCall = c.createMethodCall(iterator, iteratorClass, "HasNext")
    val nextMethodCall = c.createMethodCall(iterator, iteratorClass, "Next")

    val valInit = VarDecl(varDecl.tpe, varDecl.id, Some(nextMethodCall), varDecl.modifiers).setPos(stat)
    valInit.setSymbol(varDecl.getSymbol)
    val stats = Block(List(valInit, stat))

    c.put(While(comparisonCall, stats))
    c.getCode
  }

  //@formatter:off
  /**
    * Transforms an array slice
    *
    *   val a = <arr>[<start>:<end>]
    *
    * in to:
    *
    *   var $container = <arr>
    *   var $start = < 0|slice.start >            // 0 if slice.start is undefined
    *   var $end = < container.Size()|slice.end > // container.Size() if slice.end is undefined
    *   var $slice = new <arrTpe>[$start - $end]
    *   for(var $i = $start; i < $end; i++)
    *     $slice[$start - $i] = $container[$i]
    *   $slice
    */
  //@formatter:on
  private def desugarArraySlice(arraySlice: ArraySlice): ExprTree = {
    val sliceType = arraySlice.arr.getType
    if (sliceType.isInstanceOf[TObject])
      return arraySlice

    val c = new CodeBuilder

    val arr = arraySlice.arr
    val arrayType = arr.getType
    val arrType = arrayType.asInstanceOf[TArray].tpe
    val container = c.putVarDecl("container", arr)

    val sizeCall = c.createMethodCall(container, "Size", TInt)

    val start = c.putVarDecl("start", arraySlice.start.getOrElse(IntLit(0)))
    val end = c.putVarDecl("end", arraySlice.end.getOrElse(sizeCall).setType(TInt))

    val size = List(Minus(end, start).setType(TInt))
    val typeTree = c.getTypeTree(arrayType.asInstanceOf[TArray].tpe)
    val newArray = Trees.NewArray(typeTree, size).setType(arr)
    val slice = c.putVarDecl("slice", newArray)

    val indexDecl = c.createVarDecl("i", start)
    val indexId = indexDecl.id
    val comparison = LessThan(indexId, end).setType(TBool)
    val post = PostIncrement(indexId).setType(TInt)

    val toSlice = ArrayRead(slice, Minus(indexId, start).setType(TInt)).setType(arraySlice)
    val fromArr = ArrayRead(container, indexId).setType(arrType)
    val copyValue = Assign(toSlice, fromArr).setType(arrType)

    c.put(For(List(indexDecl), comparison, List(post), copyValue))
    c.put(slice)

    c.setPos(arraySlice)

    c.getCode.setType(sliceType)
  }
}

class CodeBuilder {
  val code = ListBuffer[StatTree]()

  def put(stat: StatTree) = {
    stat foreach {
      case t: Typed if t.getType == TUntyped =>
        sys.error(s"Tree $t does not have a type!")
      case _                                 =>
    }
    code += stat
  }

  def putVarDecl(idName: String, initExpression: ExprTree): VariableID = {
    val decl = createVarDecl(idName, initExpression)
    code += decl
    decl.id
  }

  def createMethodCall(obj: ExprTree, classSymbol: ClassSymbol, methName: String, args: List[Type] = List()): NormalAccess = {
    val methodSymbol = classSymbol.lookupMethod(methName, args).get
    createMethodCall(obj, methodSymbol)
  }

  def createMethodCall(obj: ExprTree, name: String, tpe: Type): NormalAccess =
    createMethodCall(obj, createMethodSymbol(name, tpe))

  def createMethodCall(obj: ExprTree, methodSymbol: MethodSymbol): NormalAccess = {
    val tpe = methodSymbol.getType
    val sizeMethId = createMethodId(methodSymbol)
    val mCall = MethodCall(sizeMethId, List()).setType(tpe)
    NormalAccess(obj, mCall).setType(tpe)
  }

  private def createMethodSymbol(name: String, tpe: Type) =
    new MethodSymbol(name, new ClassSymbol("", false), None, Set()).setType(tpe)

  private def createMethodId(methodSymbol: MethodSymbol): MethodID = MethodID(methodSymbol.name).setSymbol(methodSymbol)

  def createVarDecl(idName: String, initExpression: ExprTree): VarDecl = {
    val modifiers = scala.collection.immutable.Set[Modifier](Private())
    val tpe = initExpression.getType
    if (tpe == TUntyped)
      sys.error("Cannot create var decl from an untyped initial expression.")

    initExpression.setType(tpe)
    val name = '$' + idName
    val id = VariableID(name)
    val varDecl = VarDecl(None, id, Some(initExpression), modifiers)
    val symbol = new VariableSymbol(idName)
    symbol.setType(tpe)
    varDecl.setSymbol(symbol)
    id.setSymbol(symbol)
    id.setType(tpe)
    varDecl
  }

  def createOne(tpe: Type): ExprTree = tpe match {
    case TInt    => IntLit(1)
    case TChar   => IntLit(1)
    case TLong   => LongLit(1l)
    case TFloat  => FloatLit(1.0f)
    case TDouble => DoubleLit(1.0)
    case _       => ???
  }


  def getTypeTree(tpe: Type): TypeTree =
    (tpe match {
      case TUnit                => UnitType()
      case TChar                => CharType()
      case TBool                => BooleanType()
      case TInt                 => IntType()
      case TLong                => LongType()
      case TFloat               => FloatType()
      case TDouble              => DoubleType()
      case TArray(t)            => ArrayType(getTypeTree(t))
      case TObject(classSymbol) => ClassID(classSymbol.name).setSymbol(classSymbol)
      case _                    => ???
    }).setType(tpe)

  def getCode = GeneratedExpr(code.toList).setPos(code.head)

  def setPos(pos: Positioned) = code.foreach(_.setPos(pos))

}



