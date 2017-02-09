package tcompiler.analyzer

import cafebabe.AbstractByteCodes._
import cafebabe.ByteCodes._
import cafebabe.CodeHandler
import tcompiler.analyzer.Types._

object CodeMap {

  /* Types */
  val T_BOOL   = 4
  val T_CHAR   = 5
  val T_FLOAT  = 6
  val T_DOUBLE = 7
  val T_INT    = 10
  val T_LONG   = 11

}

import tcompiler.analyzer.CodeMap._

trait CodeMap {

  // Load, store create
  def load(ch: CodeHandler, index: Int): CodeHandler = ch

  def store(ch: CodeHandler, index: Int): CodeHandler = ch

  def arrayLoad(ch: CodeHandler): CodeHandler = ch

  def arrayStore(ch: CodeHandler): CodeHandler = ch

  def defaultConstant(ch: CodeHandler): CodeHandler = ch

  def one(ch: CodeHandler): CodeHandler = ch

  def newArray(ch: CodeHandler): CodeHandler = ch

  // Comparisons
  def cmpLt(ch: CodeHandler, id: String): CodeHandler = ch

  def cmpLe(ch: CodeHandler, id: String): CodeHandler = ch

  def cmpGt(ch: CodeHandler, id: String): CodeHandler = ch

  def cmpGe(ch: CodeHandler, id: String): CodeHandler = ch

  def cmpEq(ch: CodeHandler, id: String): CodeHandler = ch

  def cmpNe(ch: CodeHandler, id: String): CodeHandler = ch

  // Math
  def add(ch: CodeHandler): CodeHandler = ch

  def sub(ch: CodeHandler): CodeHandler = ch

  def mul(ch: CodeHandler): CodeHandler = ch

  def div(ch: CodeHandler): CodeHandler = ch

  def mod(ch: CodeHandler): CodeHandler = ch

  def and(ch: CodeHandler): CodeHandler = ch

  def or(ch: CodeHandler): CodeHandler = ch

  def xor(ch: CodeHandler): CodeHandler = ch

  def leftShift(ch: CodeHandler): CodeHandler = ch

  def rightShift(ch: CodeHandler): CodeHandler = ch

  // Misc
  def ret(ch: CodeHandler): CodeHandler = ch

  def negation(ch: CodeHandler): CodeHandler = ch

  def dup(ch: CodeHandler): CodeHandler = ch

  def dup_x1(ch: CodeHandler): CodeHandler = ch

  def dup_x2(ch: CodeHandler): CodeHandler = ch

  // Conversions
  def toDouble(ch: CodeHandler): CodeHandler = ch

  def toFloat(ch: CodeHandler): CodeHandler = ch

  def toLong(ch: CodeHandler): CodeHandler = ch

  def toInt(ch: CodeHandler): CodeHandler = ch

  def box(ch: CodeHandler): CodeHandler = ch

  protected def _box(ch: CodeHandler, tpe: PrimitiveType): CodeHandler = {
    val className = tpe.koolWrapper
    val typeName = tpe.byteCodeName
    ch << InvokeStatic(className, "ValueOf", s"($typeName)L$className;")
  }

}

object EmptyCodeMap extends CodeMap

object IntCodeMap extends CodeMap {
  override def load(ch: CodeHandler, index: Int): CodeHandler = ch << ILoad(index)
  override def store(ch: CodeHandler, index: Int): CodeHandler = ch << IStore(index)
  override def arrayLoad(ch: CodeHandler): CodeHandler = ch << IALOAD
  override def arrayStore(ch: CodeHandler): CodeHandler = ch << IASTORE
  override def defaultConstant(ch: CodeHandler): CodeHandler = ch << Ldc(0)
  override def one(ch: CodeHandler): CodeHandler = ch << Ldc(1)
  override def newArray(ch: CodeHandler): CodeHandler = ch << NewArray(T_INT)

  override def cmpLt(ch: CodeHandler, id: String): CodeHandler = ch << If_ICmpLt(id)
  override def cmpLe(ch: CodeHandler, id: String): CodeHandler = ch << If_ICmpLe(id)
  override def cmpGt(ch: CodeHandler, id: String): CodeHandler = ch << If_ICmpGt(id)
  override def cmpGe(ch: CodeHandler, id: String): CodeHandler = ch << If_ICmpGe(id)
  override def cmpEq(ch: CodeHandler, id: String): CodeHandler = ch << If_ICmpEq(id)
  override def cmpNe(ch: CodeHandler, id: String): CodeHandler = ch << If_ICmpNe(id)

  override def add(ch: CodeHandler): CodeHandler = ch << IADD
  override def sub(ch: CodeHandler): CodeHandler = ch << ISUB
  override def mul(ch: CodeHandler): CodeHandler = ch << IMUL
  override def div(ch: CodeHandler): CodeHandler = ch << IDIV
  override def mod(ch: CodeHandler): CodeHandler = ch << IREM

  override def and(ch: CodeHandler): CodeHandler = ch << IAND
  override def or(ch: CodeHandler): CodeHandler = ch << IOR
  override def xor(ch: CodeHandler): CodeHandler = ch << IXOR
  override def leftShift(ch: CodeHandler): CodeHandler = ch << ISHL
  override def rightShift(ch: CodeHandler): CodeHandler = ch << ISHR

  override def ret(ch: CodeHandler): CodeHandler = ch << IRETURN
  override def negation(ch: CodeHandler): CodeHandler = ch << INEG
  override def dup(ch: CodeHandler): CodeHandler = ch << DUP
  override def dup_x1(ch: CodeHandler): CodeHandler = ch << DUP_X1
  override def dup_x2(ch: CodeHandler): CodeHandler = ch << DUP_X2

  override def toDouble(ch: CodeHandler): CodeHandler = ch << I2D
  override def toFloat(ch: CodeHandler): CodeHandler = ch << I2F
  override def toLong(ch: CodeHandler): CodeHandler = ch << I2L

  override def box(ch: CodeHandler): CodeHandler = _box(ch, TInt())

}

object LongCodeMap extends CodeMap {
  override def load(ch: CodeHandler, index: Int): CodeHandler = ch << LLoad(index)
  override def store(ch: CodeHandler, index: Int): CodeHandler = ch << LStore(index)
  override def arrayLoad(ch: CodeHandler): CodeHandler = ch << LALOAD
  override def arrayStore(ch: CodeHandler): CodeHandler = ch << LASTORE
  override def defaultConstant(ch: CodeHandler): CodeHandler = ch << Ldc(0L)
  override def one(ch: CodeHandler): CodeHandler = ch << Ldc(1l)
  override def newArray(ch: CodeHandler): CodeHandler = ch << NewArray(T_LONG)

  override def cmpLt(ch: CodeHandler, id: String): CodeHandler = ch << LCMP << IfLt(id)
  override def cmpLe(ch: CodeHandler, id: String): CodeHandler = ch << LCMP << IfLe(id)
  override def cmpGt(ch: CodeHandler, id: String): CodeHandler = ch << LCMP << IfGt(id)
  override def cmpGe(ch: CodeHandler, id: String): CodeHandler = ch << LCMP << IfGe(id)
  override def cmpEq(ch: CodeHandler, id: String): CodeHandler = ch << LCMP << IfEq(id)
  override def cmpNe(ch: CodeHandler, id: String): CodeHandler = ch << LCMP << IfNe(id)

  override def add(ch: CodeHandler): CodeHandler = ch << LADD
  override def sub(ch: CodeHandler): CodeHandler = ch << LSUB
  override def mul(ch: CodeHandler): CodeHandler = ch << LMUL
  override def div(ch: CodeHandler): CodeHandler = ch << LDIV
  override def mod(ch: CodeHandler): CodeHandler = ch << LREM
  override def and(ch: CodeHandler): CodeHandler = ch << LAND
  override def or(ch: CodeHandler): CodeHandler = ch << LOR
  override def xor(ch: CodeHandler): CodeHandler = ch << LXOR
  override def leftShift(ch: CodeHandler): CodeHandler = ch << LSHL
  override def rightShift(ch: CodeHandler): CodeHandler = ch << LSHR

  override def ret(ch: CodeHandler): CodeHandler = ch << LRETURN
  override def negation(ch: CodeHandler): CodeHandler = ch << LNEG
  override def dup(ch: CodeHandler): CodeHandler = ch << DUP2
  override def dup_x1(ch: CodeHandler): CodeHandler = ch << DUP2_X1
  override def dup_x2(ch: CodeHandler): CodeHandler = ch << DUP2_X2

  override def toDouble(ch: CodeHandler): CodeHandler = ch << L2D
  override def toFloat(ch: CodeHandler): CodeHandler = ch << L2F
  override def toInt(ch: CodeHandler): CodeHandler = ch << L2I

  override def box(ch: CodeHandler): CodeHandler = _box(ch, TLong())

}

object FloatCodeMap extends CodeMap {
  override def load(ch: CodeHandler, index: Int): CodeHandler = ch << FLoad(index)
  override def store(ch: CodeHandler, index: Int): CodeHandler = ch << FStore(index)
  override def arrayLoad(ch: CodeHandler): CodeHandler = ch << FALOAD
  override def arrayStore(ch: CodeHandler): CodeHandler = ch << FASTORE
  override def defaultConstant(ch: CodeHandler): CodeHandler = ch << Ldc(0.0f)
  override def one(ch: CodeHandler): CodeHandler = ch << Ldc(1f)
  override def newArray(ch: CodeHandler): CodeHandler = ch << NewArray(T_FLOAT)

  override def cmpLt(ch: CodeHandler, id: String): CodeHandler = ch << FCMPG << IfLt(id)
  override def cmpLe(ch: CodeHandler, id: String): CodeHandler = ch << FCMPG << IfLe(id)
  override def cmpGt(ch: CodeHandler, id: String): CodeHandler = ch << FCMPL << IfGt(id)
  override def cmpGe(ch: CodeHandler, id: String): CodeHandler = ch << FCMPL << IfGe(id)
  override def cmpEq(ch: CodeHandler, id: String): CodeHandler = ch << FCMPL << IfEq(id)
  override def cmpNe(ch: CodeHandler, id: String): CodeHandler = ch << FCMPL << IfNe(id)

  override def add(ch: CodeHandler): CodeHandler = ch << FADD
  override def sub(ch: CodeHandler): CodeHandler = ch << FSUB
  override def mul(ch: CodeHandler): CodeHandler = ch << FMUL
  override def div(ch: CodeHandler): CodeHandler = ch << FDIV
  override def mod(ch: CodeHandler): CodeHandler = ch << FREM

  override def ret(ch: CodeHandler): CodeHandler = ch << FRETURN
  override def negation(ch: CodeHandler): CodeHandler = ch << FNEG
  override def dup(ch: CodeHandler): CodeHandler = ch << DUP
  override def dup_x1(ch: CodeHandler): CodeHandler = ch << DUP_X1
  override def dup_x2(ch: CodeHandler): CodeHandler = ch << DUP_X2

  override def toDouble(ch: CodeHandler): CodeHandler = ch << F2D
  override def toLong(ch: CodeHandler): CodeHandler = ch << F2L
  override def toInt(ch: CodeHandler): CodeHandler = ch << F2I

  override def box(ch: CodeHandler): CodeHandler = _box(ch, TFloat())
}

object DoubleCodeMap extends CodeMap {
  override def load(ch: CodeHandler, index: Int): CodeHandler = ch << DLoad(index)
  override def store(ch: CodeHandler, index: Int): CodeHandler = ch << DStore(index)
  override def arrayLoad(ch: CodeHandler): CodeHandler = ch << DALOAD
  override def arrayStore(ch: CodeHandler): CodeHandler = ch << DASTORE
  override def defaultConstant(ch: CodeHandler): CodeHandler = ch << Ldc(0.0)
  override def one(ch: CodeHandler): CodeHandler = ch << Ldc(1.0)
  override def newArray(ch: CodeHandler): CodeHandler = ch << NewArray(T_DOUBLE)

  override def cmpLt(ch: CodeHandler, id: String): CodeHandler = ch << DCMPG << IfLt(id)
  override def cmpLe(ch: CodeHandler, id: String): CodeHandler = ch << DCMPG << IfLe(id)
  override def cmpGt(ch: CodeHandler, id: String): CodeHandler = ch << DCMPL << IfGt(id)
  override def cmpGe(ch: CodeHandler, id: String): CodeHandler = ch << DCMPL << IfGe(id)
  override def cmpEq(ch: CodeHandler, id: String): CodeHandler = ch << DCMPL << IfEq(id)
  override def cmpNe(ch: CodeHandler, id: String): CodeHandler = ch << DCMPL << IfNe(id)

  override def add(ch: CodeHandler): CodeHandler = ch << DADD
  override def sub(ch: CodeHandler): CodeHandler = ch << DSUB
  override def mul(ch: CodeHandler): CodeHandler = ch << DMUL
  override def div(ch: CodeHandler): CodeHandler = ch << DDIV
  override def mod(ch: CodeHandler): CodeHandler = ch << DREM

  override def ret(ch: CodeHandler): CodeHandler = ch << DRETURN
  override def negation(ch: CodeHandler): CodeHandler = ch << DNEG
  override def dup(ch: CodeHandler): CodeHandler = ch << DUP2
  override def dup_x1(ch: CodeHandler): CodeHandler = ch << DUP2_X1
  override def dup_x2(ch: CodeHandler): CodeHandler = ch << DUP2_X2

  override def toFloat(ch: CodeHandler): CodeHandler = ch << D2F
  override def toLong(ch: CodeHandler): CodeHandler = ch << D2L
  override def toInt(ch: CodeHandler): CodeHandler = ch << D2I

  override def box(ch: CodeHandler): CodeHandler = _box(ch, TDouble())
}

object CharCodeMap extends CodeMap {
  override def load(ch: CodeHandler, index: Int): CodeHandler = ch << ILoad(index)
  override def store(ch: CodeHandler, index: Int): CodeHandler = ch << IStore(index)
  override def arrayLoad(ch: CodeHandler): CodeHandler = ch << CALOAD
  override def arrayStore(ch: CodeHandler): CodeHandler = ch << CASTORE
  override def defaultConstant(ch: CodeHandler): CodeHandler = ch << Ldc('\u0000')
  override def one(ch: CodeHandler): CodeHandler = ch << Ldc(1)
  override def newArray(ch: CodeHandler): CodeHandler = ch << NewArray(T_CHAR)

  override def cmpLt(ch: CodeHandler, id: String): CodeHandler = ch << If_ICmpLt(id)
  override def cmpLe(ch: CodeHandler, id: String): CodeHandler = ch << If_ICmpLe(id)
  override def cmpGt(ch: CodeHandler, id: String): CodeHandler = ch << If_ICmpGt(id)
  override def cmpGe(ch: CodeHandler, id: String): CodeHandler = ch << If_ICmpGe(id)
  override def cmpEq(ch: CodeHandler, id: String): CodeHandler = ch << If_ICmpEq(id)
  override def cmpNe(ch: CodeHandler, id: String): CodeHandler = ch << If_ICmpNe(id)

  override def add(ch: CodeHandler): CodeHandler = ch << IADD
  override def sub(ch: CodeHandler): CodeHandler = ch << ISUB
  override def mul(ch: CodeHandler): CodeHandler = ch << IMUL
  override def div(ch: CodeHandler): CodeHandler = ch << IDIV
  override def mod(ch: CodeHandler): CodeHandler = ch << IREM

  override def and(ch: CodeHandler): CodeHandler = ch << IAND
  override def or(ch: CodeHandler): CodeHandler = ch << IOR
  override def xor(ch: CodeHandler): CodeHandler = ch << IXOR
  override def leftShift(ch: CodeHandler): CodeHandler = ch << ISHL
  override def rightShift(ch: CodeHandler): CodeHandler = ch << ISHR

  override def ret(ch: CodeHandler): CodeHandler = ch << IRETURN
  override def negation(ch: CodeHandler): CodeHandler = ch << INEG
  override def dup(ch: CodeHandler): CodeHandler = ch << DUP
  override def dup_x1(ch: CodeHandler): CodeHandler = ch << DUP_X1
  override def dup_x2(ch: CodeHandler): CodeHandler = ch << DUP_X2

  override def toDouble(ch: CodeHandler): CodeHandler = ch << I2D
  override def toFloat(ch: CodeHandler): CodeHandler = ch << I2F
  override def toLong(ch: CodeHandler): CodeHandler = ch << I2L

  override def box(ch: CodeHandler): CodeHandler = _box(ch, TChar())

}

object BoolCodeMap extends CodeMap {
  override def load(ch: CodeHandler, index: Int): CodeHandler = ch << ILoad(index)
  override def store(ch: CodeHandler, index: Int): CodeHandler = ch << IStore(index)
  override def arrayLoad(ch: CodeHandler): CodeHandler = ch << BALOAD
  override def arrayStore(ch: CodeHandler): CodeHandler = ch << BASTORE
  override def defaultConstant(ch: CodeHandler): CodeHandler = ch << Ldc(0)
  override def newArray(ch: CodeHandler): CodeHandler = ch << NewArray(T_BOOL)

  override def cmpEq(ch: CodeHandler, id: String): CodeHandler = ch << If_ICmpEq(id)
  override def cmpNe(ch: CodeHandler, id: String): CodeHandler = ch << If_ICmpNe(id)

  override def and(ch: CodeHandler): CodeHandler = ch << IAND
  override def or(ch: CodeHandler): CodeHandler = ch << IOR
  override def xor(ch: CodeHandler): CodeHandler = ch << IXOR

  override def ret(ch: CodeHandler): CodeHandler = ch << IRETURN
  override def dup(ch: CodeHandler): CodeHandler = ch << DUP
  override def dup_x1(ch: CodeHandler): CodeHandler = ch << DUP_X1
  override def dup_x2(ch: CodeHandler): CodeHandler = ch << DUP_X2

  override def box(ch: CodeHandler): CodeHandler = _box(ch, TBool())

}

class ArrayCodeMap(typeName: String) extends CodeMap {
  override def load(ch: CodeHandler, index: Int): CodeHandler = ch << ALoad(index)
  override def store(ch: CodeHandler, index: Int): CodeHandler = ch << AStore(index)
  override def arrayLoad(ch: CodeHandler): CodeHandler = ch << AALOAD
  override def arrayStore(ch: CodeHandler): CodeHandler = ch << AASTORE
  override def defaultConstant(ch: CodeHandler): CodeHandler = ch << ACONST_NULL
  override def newArray(ch: CodeHandler): CodeHandler = ch << NewArray("[" + typeName)

  override def cmpEq(ch: CodeHandler, id: String): CodeHandler = ch << If_ACmpEq(id)
  override def cmpNe(ch: CodeHandler, id: String): CodeHandler = ch << If_ACmpNe(id)

  override def ret(ch: CodeHandler): CodeHandler = ch << ARETURN
  override def dup(ch: CodeHandler): CodeHandler = ch << DUP
  override def dup_x1(ch: CodeHandler): CodeHandler = ch << DUP_X1
  override def dup_x2(ch: CodeHandler): CodeHandler = ch << DUP_X2

}

class ObjectCodeMap(name: String) extends CodeMap {
  override def load(ch: CodeHandler, index: Int): CodeHandler = ch << ALoad(index)
  override def store(ch: CodeHandler, index: Int): CodeHandler = ch << AStore(index)
  override def arrayLoad(ch: CodeHandler): CodeHandler = ch << AALOAD
  override def arrayStore(ch: CodeHandler): CodeHandler = ch << AASTORE
  override def defaultConstant(ch: CodeHandler): CodeHandler = ch << ACONST_NULL
  override def newArray(ch: CodeHandler): CodeHandler = ch << NewArray(name)

  override def cmpEq(ch: CodeHandler, id: String): CodeHandler = ch << If_ACmpEq(id)
  override def cmpNe(ch: CodeHandler, id: String): CodeHandler = ch << If_ACmpNe(id)

  override def ret(ch: CodeHandler): CodeHandler = ch << ARETURN
  override def dup(ch: CodeHandler): CodeHandler = ch << DUP
  override def dup_x1(ch: CodeHandler): CodeHandler = ch << DUP_X1
  override def dup_x2(ch: CodeHandler): CodeHandler = ch << DUP_X2

}