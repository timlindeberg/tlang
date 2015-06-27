package koolc.analyzer

import cafebabe.AbstractByteCodes._
import cafebabe.ByteCodes._
import cafebabe.CodeHandler

abstract class CodeMap {

  /* Types */
  val T_INT = 10
  val T_BOOL = 4

  // Load, store create
  def load(ch: CodeHandler, index: Int): CodeHandler
  def store(ch: CodeHandler, index: Int): CodeHandler
  def arrayLoad(ch: CodeHandler): CodeHandler
  def arrayStore(ch: CodeHandler): CodeHandler
  def defaultConstant(ch: CodeHandler): CodeHandler
  def newArray(ch: CodeHandler): CodeHandler

  // Comparisons
  def cmpLt(ch: CodeHandler, id: String): CodeHandler
  def cmpLe(ch: CodeHandler, id: String): CodeHandler
  def cmpGe(ch: CodeHandler, id: String): CodeHandler
  def cmpGt(ch: CodeHandler, id: String): CodeHandler
  def cmpEq(ch: CodeHandler, id: String): CodeHandler
  def cmpNe(ch: CodeHandler, id: String): CodeHandler

  // Math
  def add(ch: CodeHandler): CodeHandler
  def sub(ch: CodeHandler): CodeHandler
  def mul(ch: CodeHandler): CodeHandler
  def div(ch: CodeHandler): CodeHandler
  def mod(ch: CodeHandler): CodeHandler
  def and(ch: CodeHandler): CodeHandler
  def or(ch: CodeHandler): CodeHandler
  def xor(ch: CodeHandler): CodeHandler
  def leftShift(ch: CodeHandler): CodeHandler
  def rightShift(ch: CodeHandler): CodeHandler

  // Misc
  def ret(ch: CodeHandler): CodeHandler
  def negation(ch: CodeHandler): CodeHandler
}

object EmptyCodeMap extends CodeMap {
  override def load(ch: CodeHandler, index: Int) = ch
  override def store(ch: CodeHandler, index: Int) = ch
  override def arrayLoad(ch: CodeHandler) = ch
  override def arrayStore(ch: CodeHandler) = ch
  override def defaultConstant(ch: CodeHandler) = ch
  override def newArray(ch: CodeHandler) = ch

  override def cmpLt(ch: CodeHandler, id: String) = ch
  override def cmpLe(ch: CodeHandler, id: String) = ch
  override def cmpGe(ch: CodeHandler, id: String) = ch
  override def cmpGt(ch: CodeHandler, id: String) = ch
  override def cmpEq(ch: CodeHandler, id: String) = ch
  override def cmpNe(ch: CodeHandler, id: String) = ch

  override def add(ch: CodeHandler) = ch
  override def sub(ch: CodeHandler) = ch
  override def mul(ch: CodeHandler) = ch
  override def div(ch: CodeHandler) = ch
  override def mod(ch: CodeHandler) = ch
  override def and(ch: CodeHandler) = ch
  override def or(ch: CodeHandler) = ch
  override def xor(ch: CodeHandler) = ch
  override def leftShift(ch: CodeHandler) = ch
  override def rightShift(ch: CodeHandler) = ch

  override def ret(ch: CodeHandler) = ch
  override def negation(ch: CodeHandler) = ch
}

object IntCodeMap extends CodeMap {
  override def load(ch: CodeHandler, index: Int) = ch << ILoad(index)
  override def store(ch: CodeHandler, index: Int) = ch << IStore(index)
  override def arrayLoad(ch: CodeHandler) = ch << IALOAD
  override def arrayStore(ch: CodeHandler) = ch << IASTORE
  override def defaultConstant(ch: CodeHandler) = ch << Ldc(0)
  override def newArray(ch: CodeHandler) = ch << NewArray(T_INT)

  override def cmpLt(ch: CodeHandler, id: String) = ch << If_ICmpLt(id)
  override def cmpLe(ch: CodeHandler, id: String) = ch << If_ICmpLe(id)
  override def cmpGe(ch: CodeHandler, id: String) = ch << If_ICmpGe(id)
  override def cmpGt(ch: CodeHandler, id: String) = ch << If_ICmpGt(id)
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
}

object BoolCodeMap extends CodeMap {
  override def load(ch: CodeHandler, index: Int) = ch << ILoad(index)
  override def store(ch: CodeHandler, index: Int) = ch << IStore(index)
  override def arrayLoad(ch: CodeHandler) = ch << BALOAD
  override def arrayStore(ch: CodeHandler) = ch << BASTORE
  override def defaultConstant(ch: CodeHandler) = ch << Ldc(0)
  override def newArray(ch: CodeHandler) = ch << NewArray(T_BOOL)

  override def cmpLt(ch: CodeHandler, id: String) = ch << If_ICmpLt(id)
  override def cmpLe(ch: CodeHandler, id: String) = ch << If_ICmpLe(id)
  override def cmpGe(ch: CodeHandler, id: String) = ch << If_ICmpGe(id)
  override def cmpGt(ch: CodeHandler, id: String) = ch << If_ICmpGt(id)
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
  override def negation(ch: CodeHandler) = ch
}

object StringCodeMap extends CodeMap {
  override def load(ch: CodeHandler, index: Int) = ch << ALoad(index)
  override def store(ch: CodeHandler, index: Int) = ch << AStore(index)
  override def arrayLoad(ch: CodeHandler) = ch << AALOAD
  override def arrayStore(ch: CodeHandler) = ch << AASTORE
  override def defaultConstant(ch: CodeHandler) = ch << ACONST_NULL
  override def newArray(ch: CodeHandler) = ch << NewArray("java/lang/String")

  override def cmpLt(ch: CodeHandler, id: String) = ch
  override def cmpLe(ch: CodeHandler, id: String) = ch
  override def cmpGe(ch: CodeHandler, id: String) = ch
  override def cmpGt(ch: CodeHandler, id: String) = ch
  override def cmpEq(ch: CodeHandler, id: String) = ch << If_ACmpEq(id)
  override def cmpNe(ch: CodeHandler, id: String) = ch << If_ACmpNe(id)

  override def add(ch: CodeHandler) = ch
  override def sub(ch: CodeHandler) = ch
  override def mul(ch: CodeHandler) = ch
  override def div(ch: CodeHandler) = ch
  override def mod(ch: CodeHandler) = ch
  override def and(ch: CodeHandler) = ch
  override def or(ch: CodeHandler) = ch
  override def xor(ch: CodeHandler) = ch
  override def leftShift(ch: CodeHandler) = ch
  override def rightShift(ch: CodeHandler) = ch

  override def ret(ch: CodeHandler) = ch << ARETURN
  override def negation(ch: CodeHandler) = ch
}

object ArrayCodeMap extends CodeMap {
  override def load(ch: CodeHandler, index: Int) = ch << ALoad(index)
  override def store(ch: CodeHandler, index: Int) = ch << AStore(index)
  override def arrayLoad(ch: CodeHandler) = ???
  override def arrayStore(ch: CodeHandler) = ???
  override def defaultConstant(ch: CodeHandler) = ch << ACONST_NULL
  override def newArray(ch: CodeHandler) = ???

  override def cmpLt(ch: CodeHandler, id: String) = ch
  override def cmpLe(ch: CodeHandler, id: String) = ch
  override def cmpGe(ch: CodeHandler, id: String) = ch
  override def cmpGt(ch: CodeHandler, id: String) = ch
  override def cmpEq(ch: CodeHandler, id: String) = ch << If_ACmpEq(id)
  override def cmpNe(ch: CodeHandler, id: String) = ch << If_ACmpNe(id)

  override def add(ch: CodeHandler) = ch
  override def sub(ch: CodeHandler) = ch
  override def mul(ch: CodeHandler) = ch
  override def div(ch: CodeHandler) = ch
  override def mod(ch: CodeHandler) = ch
  override def and(ch: CodeHandler) = ch
  override def or(ch: CodeHandler) = ch
  override def xor(ch: CodeHandler) = ch
  override def leftShift(ch: CodeHandler) = ch
  override def rightShift(ch: CodeHandler) = ch

  override def ret(ch: CodeHandler) = ch << ARETURN
  override def negation(ch: CodeHandler) = ch
}

class ObjectCodeMap(name: String) extends CodeMap {
  override def load(ch: CodeHandler, index: Int) = ch << ALoad(index)
  override def store(ch: CodeHandler, index: Int) = ch << AStore(index)
  override def arrayLoad(ch: CodeHandler) = ch << AALOAD
  override def arrayStore(ch: CodeHandler) = ch << AASTORE
  override def defaultConstant(ch: CodeHandler) = ch << ACONST_NULL
  override def newArray(ch: CodeHandler) = ch << NewArray(name)

  override def cmpLt(ch: CodeHandler, id: String) = ch
  override def cmpLe(ch: CodeHandler, id: String) = ch
  override def cmpGe(ch: CodeHandler, id: String) = ch
  override def cmpGt(ch: CodeHandler, id: String) = ch
  override def cmpEq(ch: CodeHandler, id: String) = ch << If_ACmpEq(id)
  override def cmpNe(ch: CodeHandler, id: String) = ch << If_ACmpNe(id)

  override def add(ch: CodeHandler) = ch
  override def sub(ch: CodeHandler) = ch
  override def mul(ch: CodeHandler) = ch
  override def div(ch: CodeHandler) = ch
  override def mod(ch: CodeHandler) = ch
  override def and(ch: CodeHandler) = ch
  override def or(ch: CodeHandler) = ch
  override def xor(ch: CodeHandler) = ch
  override def leftShift(ch: CodeHandler) = ch
  override def rightShift(ch: CodeHandler) = ch

  override def ret(ch: CodeHandler) = ch << ARETURN
  override def negation(ch: CodeHandler) = ch
}