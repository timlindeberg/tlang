package tcompiler.analyzer

import cafebabe.AbstractByteCodes._
import cafebabe.ByteCodes._
import cafebabe.CodeHandler

trait CodeMap {

  /* Types */
  val T_BOOL = 4
  val T_CHAR = 5
  val T_FLOAT = 6
  val T_DOUBLE = 7
  val T_INT = 10
  val T_LONG = 11



  // Load, store create
  def load(ch: CodeHandler, index: Int): CodeHandler = ch
  def store(ch: CodeHandler, index: Int): CodeHandler = ch
  def arrayLoad(ch: CodeHandler): CodeHandler = ch
  def arrayStore(ch: CodeHandler): CodeHandler = ch
  def defaultConstant(ch: CodeHandler): CodeHandler = ch
  def one(ch: CodeHandler) = ch

  def newArray(ch: CodeHandler): CodeHandler = ch

  // Comparisons
  def cmpLt(ch: CodeHandler, id: String): CodeHandler = ch
  def cmpLe(ch: CodeHandler, id: String): CodeHandler = ch
  def cmpGt(ch: CodeHandler, id: String): CodeHandler = ch
  def cmpGe(ch: CodeHandler, id: String): CodeHandler = ch
  def cmpEq(ch: CodeHandler, id: String): CodeHandler = ch
  def cmpNe(ch: CodeHandler, id: String): CodeHandler = ch

  // Math
  def add(ch: CodeHandler): CodeHandler  = ch
  def sub(ch: CodeHandler): CodeHandler = ch
  def mul(ch: CodeHandler): CodeHandler = ch
  def div(ch: CodeHandler): CodeHandler = ch
  def mod(ch: CodeHandler): CodeHandler = ch

  def and(ch: CodeHandler): CodeHandler  = ch
  def or(ch: CodeHandler): CodeHandler = ch
  def xor(ch: CodeHandler): CodeHandler  = ch
  def leftShift(ch: CodeHandler): CodeHandler  = ch
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

}

object EmptyCodeMap extends CodeMap

object IntCodeMap extends CodeMap {
  override def load(ch: CodeHandler, index: Int) = ch << ILoad(index)
  override def store(ch: CodeHandler, index: Int) = ch << IStore(index)
  override def arrayLoad(ch: CodeHandler) = ch << IALOAD
  override def arrayStore(ch: CodeHandler) = ch << IASTORE
  override def defaultConstant(ch: CodeHandler) = ch << Ldc(0)
  override def one(ch: CodeHandler) = ch << Ldc(1)
  override def newArray(ch: CodeHandler) = ch << NewArray(T_INT)

  override def cmpLt(ch: CodeHandler, id: String) = ch << If_ICmpLt(id)
  override def cmpLe(ch: CodeHandler, id: String) = ch << If_ICmpLe(id)
  override def cmpGt(ch: CodeHandler, id: String) = ch << If_ICmpGt(id)
  override def cmpGe(ch: CodeHandler, id: String) = ch << If_ICmpGe(id)
  override def cmpEq(ch: CodeHandler, id: String) = ch << If_ICmpEq(id)
  override def cmpNe(ch: CodeHandler, id: String) = ch << If_ICmpNe(id)

  override def add(ch: CodeHandler) = ch << IADD
  override def sub(ch: CodeHandler) = ch << ISUB
  override def mul(ch: CodeHandler) = ch << IMUL
  override def div(ch: CodeHandler) = ch << IDIV
  override def mod(ch: CodeHandler) = ch << IREM

  override def and(ch: CodeHandler) = ch << IAND
  override def or(ch: CodeHandler) = ch << IOR
  override def xor(ch: CodeHandler) = ch << IXOR
  override def leftShift(ch: CodeHandler) = ch << ISHL
  override def rightShift(ch: CodeHandler) = ch << ISHR

  override def ret(ch: CodeHandler) = ch << IRETURN
  override def negation(ch: CodeHandler) = ch << INEG
  override def dup(ch: CodeHandler) = ch << DUP
  override def dup_x1(ch: CodeHandler): CodeHandler = ch << DUP_X1
  override def dup_x2(ch: CodeHandler): CodeHandler = ch << DUP_X2

  override def toDouble(ch: CodeHandler) = ch << I2D
  override def toFloat(ch: CodeHandler) = ch << I2F
  override def toLong(ch: CodeHandler) = ch << I2L
}

object LongCodeMap extends CodeMap {
  override def load(ch: CodeHandler, index: Int) = ch << LLoad(index)
  override def store(ch: CodeHandler, index: Int) = ch << LStore(index)
  override def arrayLoad(ch: CodeHandler) = ch << LALOAD
  override def arrayStore(ch: CodeHandler) = ch << LASTORE
  override def defaultConstant(ch: CodeHandler) = ch << Ldc(0L)
  override def one(ch: CodeHandler) = ch << Ldc(1l)
  override def newArray(ch: CodeHandler) = ch << NewArray(T_LONG)

  override def cmpLt(ch: CodeHandler, id: String) = ch << LCMP << IfLt(id)
  override def cmpLe(ch: CodeHandler, id: String) = ch << LCMP << IfLe(id)
  override def cmpGt(ch: CodeHandler, id: String) = ch << LCMP << IfGt(id)
  override def cmpGe(ch: CodeHandler, id: String) = ch << LCMP << IfGe(id)
  override def cmpEq(ch: CodeHandler, id: String) = ch << LCMP << IfEq(id)
  override def cmpNe(ch: CodeHandler, id: String) = ch << LCMP << IfNe(id)

  override def add(ch: CodeHandler) = ch << LADD
  override def sub(ch: CodeHandler) = ch << LSUB
  override def mul(ch: CodeHandler) = ch << LMUL
  override def div(ch: CodeHandler) = ch << LDIV
  override def mod(ch: CodeHandler) = ch << LREM
  override def and(ch: CodeHandler) = ch << LAND
  override def or(ch: CodeHandler) = ch << LOR
  override def xor(ch: CodeHandler) = ch << LXOR
  override def leftShift(ch: CodeHandler) = ch << LSHL
  override def rightShift(ch: CodeHandler) = ch << LSHR

  override def ret(ch: CodeHandler) = ch << LRETURN
  override def negation(ch: CodeHandler) = ch << LNEG
  override def dup(ch: CodeHandler) = ch << DUP2
  override def dup_x1(ch: CodeHandler): CodeHandler = ch << DUP2_X1
  override def dup_x2(ch: CodeHandler): CodeHandler = ch << DUP2_X2

  override def toDouble(ch: CodeHandler) = ch << L2D
  override def toFloat(ch: CodeHandler) = ch << L2F
  override def toInt(ch: CodeHandler) = ch << L2I

}

object FloatCodeMap extends CodeMap {
  override def load(ch: CodeHandler, index: Int) = ch << FLoad(index)
  override def store(ch: CodeHandler, index: Int) = ch << FStore(index)
  override def arrayLoad(ch: CodeHandler) = ch << FALOAD
  override def arrayStore(ch: CodeHandler) = ch << FASTORE
  override def defaultConstant(ch: CodeHandler) = ch << Ldc(0.0f)
  override def one(ch: CodeHandler) = ch << Ldc(1f)
  override def newArray(ch: CodeHandler) = ch << NewArray(T_FLOAT)

  override def cmpLt(ch: CodeHandler, id: String) = ch << FCMPG << IfLt(id)
  override def cmpLe(ch: CodeHandler, id: String) = ch << FCMPG << IfLe(id)
  override def cmpGt(ch: CodeHandler, id: String) = ch << FCMPL << IfGt(id)
  override def cmpGe(ch: CodeHandler, id: String) = ch << FCMPL << IfGe(id)
  override def cmpEq(ch: CodeHandler, id: String) = ch << FCMPL << IfEq(id)
  override def cmpNe(ch: CodeHandler, id: String) = ch << FCMPL << IfNe(id)

  override def add(ch: CodeHandler) = ch << FADD
  override def sub(ch: CodeHandler) = ch << FSUB
  override def mul(ch: CodeHandler) = ch << FMUL
  override def div(ch: CodeHandler) = ch << FDIV
  override def mod(ch: CodeHandler) = ch << FREM

  override def ret(ch: CodeHandler) = ch << FRETURN
  override def negation(ch: CodeHandler) = ch << FNEG
  override def dup(ch: CodeHandler) = ch << DUP
  override def dup_x1(ch: CodeHandler): CodeHandler = ch << DUP_X1
  override def dup_x2(ch: CodeHandler): CodeHandler = ch << DUP_X2

  override def toDouble(ch: CodeHandler) = ch << F2D
  override def toLong(ch: CodeHandler) = ch << F2L
  override def toInt(ch: CodeHandler) = ch << F2I
}

object DoubleCodeMap extends CodeMap {
  override def load(ch: CodeHandler, index: Int) = ch << DLoad(index)
  override def store(ch: CodeHandler, index: Int) = ch << DStore(index)
  override def arrayLoad(ch: CodeHandler) = ch << DALOAD
  override def arrayStore(ch: CodeHandler) = ch << DASTORE
  override def defaultConstant(ch: CodeHandler) = ch << Ldc(0.0)
  override def one(ch: CodeHandler) = ch << Ldc(1.0)
  override def newArray(ch: CodeHandler) = ch << NewArray(T_DOUBLE)

  override def cmpLt(ch: CodeHandler, id: String) = ch << DCMPG << IfLt(id)
  override def cmpLe(ch: CodeHandler, id: String) = ch << DCMPG << IfLe(id)
  override def cmpGt(ch: CodeHandler, id: String) = ch << DCMPL << IfGt(id)
  override def cmpGe(ch: CodeHandler, id: String) = ch << DCMPL << IfGe(id)
  override def cmpEq(ch: CodeHandler, id: String) = ch << DCMPL << IfEq(id)
  override def cmpNe(ch: CodeHandler, id: String) = ch << DCMPL << IfNe(id)

  override def add(ch: CodeHandler) = ch << DADD
  override def sub(ch: CodeHandler) = ch << DSUB
  override def mul(ch: CodeHandler) = ch << DMUL
  override def div(ch: CodeHandler) = ch << DDIV
  override def mod(ch: CodeHandler) = ch << DREM

  override def ret(ch: CodeHandler) = ch << DRETURN
  override def negation(ch: CodeHandler) = ch << DNEG
  override def dup(ch: CodeHandler) = ch << DUP2
  override def dup_x1(ch: CodeHandler): CodeHandler = ch << DUP2_X1
  override def dup_x2(ch: CodeHandler): CodeHandler = ch << DUP2_X2

  override def toFloat(ch: CodeHandler) = ch << D2F
  override def toLong(ch: CodeHandler) = ch << D2L
  override def toInt(ch: CodeHandler) = ch << D2I
}

object CharCodeMap extends CodeMap {
  override def load(ch: CodeHandler, index: Int) = ch << ILoad(index)
  override def store(ch: CodeHandler, index: Int) = ch << IStore(index)
  override def arrayLoad(ch: CodeHandler) = ch << CALOAD
  override def arrayStore(ch: CodeHandler) = ch << CASTORE
  override def defaultConstant(ch: CodeHandler) = ch << Ldc('\u0000')
  override def one(ch: CodeHandler) = ch << Ldc(1)
  override def newArray(ch: CodeHandler) = ch << NewArray(T_CHAR)

  override def cmpLt(ch: CodeHandler, id: String) = ch << If_ICmpLt(id)
  override def cmpLe(ch: CodeHandler, id: String) = ch << If_ICmpLe(id)
  override def cmpGt(ch: CodeHandler, id: String) = ch << If_ICmpGt(id)
  override def cmpGe(ch: CodeHandler, id: String) = ch << If_ICmpGe(id)
  override def cmpEq(ch: CodeHandler, id: String) = ch << If_ICmpEq(id)
  override def cmpNe(ch: CodeHandler, id: String) = ch << If_ICmpNe(id)

  override def add(ch: CodeHandler) = ch << IADD
  override def sub(ch: CodeHandler) = ch << ISUB
  override def mul(ch: CodeHandler) = ch << IMUL
  override def div(ch: CodeHandler) = ch << IDIV
  override def mod(ch: CodeHandler) = ch << IREM

  override def and(ch: CodeHandler) = ch << IAND
  override def or(ch: CodeHandler) = ch << IOR
  override def xor(ch: CodeHandler) = ch << IXOR
  override def leftShift(ch: CodeHandler) = ch << ISHL
  override def rightShift(ch: CodeHandler) = ch << ISHR

  override def ret(ch: CodeHandler) = ch << IRETURN
  override def negation(ch: CodeHandler) = ch << INEG
  override def dup(ch: CodeHandler) = ch << DUP
  override def dup_x1(ch: CodeHandler): CodeHandler = ch << DUP_X1
  override def dup_x2(ch: CodeHandler): CodeHandler = ch << DUP_X2

  override def toDouble(ch: CodeHandler) = ch << I2D
  override def toFloat(ch: CodeHandler) = ch << I2F
  override def toLong(ch: CodeHandler) = ch << I2L
}

object BoolCodeMap extends CodeMap {
  override def load(ch: CodeHandler, index: Int) = ch << ILoad(index)
  override def store(ch: CodeHandler, index: Int) = ch << IStore(index)
  override def arrayLoad(ch: CodeHandler) = ch << BALOAD
  override def arrayStore(ch: CodeHandler) = ch << BASTORE
  override def defaultConstant(ch: CodeHandler) = ch << Ldc(0)
  override def newArray(ch: CodeHandler) = ch << NewArray(T_BOOL)

  override def cmpEq(ch: CodeHandler, id: String) = ch << If_ICmpEq(id)
  override def cmpNe(ch: CodeHandler, id: String) = ch << If_ICmpNe(id)

  override def and(ch: CodeHandler) = ch << IAND
  override def or(ch: CodeHandler) = ch << IOR
  override def xor(ch: CodeHandler) = ch << IXOR

  override def ret(ch: CodeHandler) = ch << IRETURN
  override def dup(ch: CodeHandler) = ch << DUP
  override def dup_x1(ch: CodeHandler): CodeHandler = ch << DUP_X1
  override def dup_x2(ch: CodeHandler): CodeHandler = ch << DUP_X2

}

object StringCodeMap extends CodeMap {
  override def load(ch: CodeHandler, index: Int) = ch << ALoad(index)
  override def store(ch: CodeHandler, index: Int) = ch << AStore(index)
  override def arrayLoad(ch: CodeHandler) = ch << AALOAD
  override def arrayStore(ch: CodeHandler) = ch << AASTORE
  override def defaultConstant(ch: CodeHandler) = ch << ACONST_NULL
  override def newArray(ch: CodeHandler) = ch << NewArray("java/lang/String")

  override def cmpEq(ch: CodeHandler, id: String) = ch << If_ACmpEq(id)
  override def cmpNe(ch: CodeHandler, id: String) = ch << If_ACmpNe(id)

  override def ret(ch: CodeHandler) = ch << ARETURN
  override def dup(ch: CodeHandler) = ch << DUP
  override def dup_x1(ch: CodeHandler): CodeHandler = ch << DUP_X1
  override def dup_x2(ch: CodeHandler): CodeHandler = ch << DUP_X2

}

object ArrayCodeMap extends CodeMap {
  override def load(ch: CodeHandler, index: Int) = ch << ALoad(index)
  override def store(ch: CodeHandler, index: Int) = ch << AStore(index)
  override def arrayLoad(ch: CodeHandler) = ???
  override def arrayStore(ch: CodeHandler) = ???
  override def defaultConstant(ch: CodeHandler) = ch << ACONST_NULL
  override def newArray(ch: CodeHandler) = ???

  override def cmpEq(ch: CodeHandler, id: String) = ch << If_ACmpEq(id)
  override def cmpNe(ch: CodeHandler, id: String) = ch << If_ACmpNe(id)

  override def ret(ch: CodeHandler) = ch << ARETURN
  override def dup(ch: CodeHandler) = ch << DUP
  override def dup_x1(ch: CodeHandler): CodeHandler = ch << DUP_X1
  override def dup_x2(ch: CodeHandler): CodeHandler = ch << DUP_X2

}

class ObjectCodeMap(name: String) extends CodeMap {
  override def load(ch: CodeHandler, index: Int) = ch << ALoad(index)
  override def store(ch: CodeHandler, index: Int) = ch << AStore(index)
  override def arrayLoad(ch: CodeHandler) = ch << AALOAD
  override def arrayStore(ch: CodeHandler) = ch << AASTORE
  override def defaultConstant(ch: CodeHandler) = ch << ACONST_NULL
  override def newArray(ch: CodeHandler) = ch << NewArray(name)

  override def cmpEq(ch: CodeHandler, id: String) = ch << If_ACmpEq(id)
  override def cmpNe(ch: CodeHandler, id: String) = ch << If_ACmpNe(id)

  override def ret(ch: CodeHandler) = ch << ARETURN
  override def dup(ch: CodeHandler) = ch << DUP
  override def dup_x1(ch: CodeHandler): CodeHandler = ch << DUP_X1
  override def dup_x2(ch: CodeHandler): CodeHandler = ch << DUP_X2

}