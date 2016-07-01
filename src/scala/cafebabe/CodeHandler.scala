package cafebabe

import scala.collection.mutable.{ListBuffer, Map => MutableMap}

/** A code handler contains methods to help the generation of method bodies.
  * The general usage is to generate abstract byte codes, then freeze the code,
  * which as a consequence generates the proper bytes in the CodeAttributeInfo.
  * Information is added to the constant pool during the ABS generation already.
  * <code>CodeHandler</code>s should not be created manually, but rather obtained
  * from the corresponding <code>MethodHandler</code>. */
class CodeHandler private[cafebabe](c: CodeAttributeInfo, cp: ConstantPool, val paramTypes: String, val isStatic: Boolean) {

  import AbstractByteCodes._
  import ByteCodes._
  import ClassFileTypes._

  private             val code        : CodeAttributeInfo            = c
  protected[cafebabe] val constantPool: ConstantPool                 = cp
  // will be built backwards and reversed at the end...
  private             val abcBuffer   : ListBuffer[AbstractByteCode] = ListBuffer.empty
  private             var frozen      : Boolean                      = false
  protected[cafebabe] def isFrozen: Boolean = frozen

  def peek: Option[AbstractByteCode] = abcBuffer.headOption

  def lastRealInstruction: Option[AbstractByteCode] = {
    def lastRealInstruction(list: ListBuffer[AbstractByteCode]): Option[AbstractByteCode] = {
      if (list.isEmpty) None
      else if (list.head.isInstanceOf[LineNumber]) lastRealInstruction(list.tail)
      else Some(list.head)
    }

    lastRealInstruction(abcBuffer.reverse)
  }

  private def append(abc: AbstractByteCode): Unit = if (!frozen) {
    abcBuffer += abc
  }

  def <<(abcGen: AbstractByteCodeGenerator): CodeHandler = {
    abcGen(this)
  }

  def <<(abc: AbstractByteCode): CodeHandler = {
    append(abc)
    this
  }

  // Helpers to get slots.
  private             val argTypesAndBytes: Seq[(String, Int)]      = {
    val bc = typesToTypesAndBytes(paramTypes)
    // that's a dirty trick, but this info is very private..
    // only used for the ArgLoad ABC.
    if (isStatic) bc else ("L;", 1) +: bc
  }
  protected[cafebabe] val argSlotMap      : Map[Int, (String, Int)] = {
    var acc: Int = 0
    (for (((tpe, sz), arg) <- argTypesAndBytes.zipWithIndex) yield {
      val s = acc
      acc += sz
      (arg, (tpe, s))
    }).toMap
  }
  private             var locals          : Int                     = argTypesAndBytes.unzip._2.sum

  /** Get a slot for a var that fits in one byte (all but `double` or `long`). */
  def getFreshVar: Int = getFreshVar(1)

  /** Get a slot for a var whose type has the JVM representation `tpe`. */
  def getFreshVar(tpe: String): Int = getFreshVar(typeToByteCount(tpe))

  /** Get a slot for var that fits in `n` bytes. `n` must be `0` or `1`. */
  def getFreshVar(n: Int): Int = {
    if (!(n == 1 || n == 2)) {
      throw new IllegalArgumentException("Slot for variables can only be of 1 or 2 bytes.")
    }
    val ret = locals
    locals += n
    ret
  }

  @deprecated("freeVar no longer has any effect", "1.3")
  def freeVar(id: Int): Unit = {}

  // Helper to get fresh label names.
  private var labelCounts = new scala.collection.mutable.HashMap[String, Int]
  def getFreshLabel(prefix: String): String = {
    val postfix: Int = labelCounts.getOrElse(prefix, {
      labelCounts(prefix) = 0
      0
    })
    val name = prefix + '_' + postfix
    labelCounts(prefix) = postfix + 1
    name
  }

  /** "Freezes" the code: maxLocals is computed, abstract byte codes are turned
    * into concrete ones. This includes computation of the label offsets. */
  def freeze: Unit = if (frozen) {
    throw CodeFreezingException(
      "Cannot invoke `freeze` twice on the same CodeHandler.")
  } else {
    frozen = true

    val abcList = abcBuffer.toList
    code.maxLocals = locals

    val labels: MutableMap[String, Int] = MutableMap.empty
    val lineInfo: MutableMap[Int, Int] = MutableMap.empty

    // In the first pass, we collect the positions of all labels.
    // We also store line numbers information.
    locally {
      var pc: Int = 0
      var lastLineNumber: Int = Int.MaxValue
      for (abc <- abcList) {
        abc match {
          case LineNumber(ln) if ln == lastLineNumber => ;
          case LineNumber(ln)                         => {
            lastLineNumber = ln
            lineInfo(pc) = ln
          }
          case Label(name)                            => labels(name) = pc
          case _                                      => ;
        }
        pc = pc + abc.size
      }
    }

    // In the second pass, we set the jump offsets.
    locally {
      var pc: Int = 0
      for (abc <- abcList) {
        abc match {
          case co: ControlOperator => {
            co.offset = labels.getOrElse(co.target, 0) - pc
          }
          case _                   => ;
        }
        pc = pc + abc.size
      }
    }




    // we build the line number table.
    if (lineInfo.nonEmpty) {
      val lnta = new LineNumberTableAttributeInfo(constantPool.addString("LineNumberTable"))
      lnta.setEntries(lineInfo.toSeq)
      code.attributes = lnta +: code.attributes
    }

    // we now compute the maximum stack height.
    code.maxStack = computeMaxStack(abcList)

    // finally, we dump the code.
    abcList.foreach(code.code << _)
  }

  def computeMaxStack(abcList: List[AbstractByteCode]): U2 = {
    // This should get called exactly once, at freezing time.
    assert(frozen)

    val actualSize = abcList.map(_.size).sum
    val codeArray = new Array[AbstractByteCode](actualSize)

    locally {
      var pc = 0
      for (abc <- abcList) {
        codeArray(pc) = abc
        pc += abc.size
      }
    }

    val UninitializedHeight: Int = Int.MinValue
    val heightArray = Array.fill[Int](actualSize)(UninitializedHeight)

    def stackTrace: String = {
      if (abcBuffer.isEmpty)
        return ""

      val types = Map(
        4 -> "T_BOOLEAN",
        5 -> "T_CHAR",
        6 -> "T_FLOAT",
        7 -> "T_DOUBLE",
        8 -> "T_BYTE",
        9 -> "T_SHORT",
        10 -> "T_INT",
        11 -> "T_LONG"
      )

      val b = new StringBuilder()
      var pc = 0
      var currentLineNumber = 0

      def appendAbc(abc: AbstractByteCode, extraInfo: String) = {
        val h = heightArray(pc)
        val height = if (h == UninitializedHeight) "?" else String.valueOf(h)
        b.append("%4d %5d %6s %-15s %s\n".format(currentLineNumber, pc, height, abc, extraInfo))
      }

      b.append("%4s %5s %6s %-15s %s\n".format("Line", "PC", "Height", "ByteCode", "ExtraInfo"))
      var i = 0
      while (i < abcBuffer.size) {
        val abc = abcBuffer(i)
        abc match {
          case Label(name)              =>
            b.append("%17s:\n".format(name))
          case LineNumber(line)         =>
            currentLineNumber = line
          case _: RawByte | _: RawBytes =>
          case NEWARRAY                 =>
            abcBuffer(i + 1) match {
              case RawByte(tpe) => appendAbc(abc, types(tpe))
              case _            => ???
            }
          case IINC =>
            abcBuffer(i + 1) match {
              case RawByte(index) => abcBuffer(i + 2) match {
                case RawByte(amount) => appendAbc(abc, s"$index $amount")
                case _ => ???
              }
              case _            => ???
            }
          case BIPUSH =>
            abcBuffer(i + 1) match {
              case RawByte(amount) => appendAbc(abc, s"$amount")
              case _            => ???
            }
          case SIPUSH =>
            abcBuffer(i + 1) match {
              case RawBytes(amount) => appendAbc(abc, s"$amount")
              case _            => ???
            }
          case ALOAD | ILOAD | FLOAD | LLOAD | DLOAD |
               ASTORE | ISTORE | FSTORE | LSTORE | DSTORE =>
            abcBuffer(i + 1) match {
              case RawByte(index) => appendAbc(abc, s"$index")
              case _            => ???
            }
          case _                        =>
            var extraInfo = ""
            if(i + 1 < abcBuffer.size){
              abcBuffer(i + 1) match {
                case RawByte(idx)  => extraInfo += cp.getByteInfo(idx)
                case RawBytes(idx) => extraInfo += cp.getByteInfo(idx)
                case _             =>
              }
            }
            appendAbc(abc, extraInfo)
        }
        pc += abc.size
        i += 1
      }
      b.append("\n")
      b.toString
    }

    // An invocation of this function reads as "when the pc reaches `from`, the stack height should be `there`".
    def setHeight(from: Int, there: Int): Unit = {
      if (from < 0 || from >= actualSize)
        throw CodeFreezingException("No bytecode at pc=" + from + ". Missing instructions?\n" + stackTrace)

      if (there < 0) {
        heightArray(from) = there
        throw CodeFreezingException("Negative stack height (" + there + ") at pc=" + from + " (which is " + codeArray(from) + ").\n" + stackTrace)
      }

      if (heightArray(from) != UninitializedHeight) {
        // If another paths led to the same pc.
        if (heightArray(from) == there) {
          return
        } else {
          throw CodeFreezingException("Inconsistent stack height at pc=" + from + "(" + heightArray(from) + " and " + there + ")\n" + stackTrace)
        }
      }

      val pc = from
      heightArray(pc) = there

      codeArray(pc) match {
        case WIDE                                            => sys.error("Wide is unsupported for now.")
        case RETURN                                          => if (there != 0) throw CodeFreezingException("Non-empty stack after return in void method\n" + stackTrace)
        case ATHROW                                          => {
          // Nothing really matters.
        }
        case ARETURN | DRETURN | FRETURN | IRETURN | LRETURN => {
          if (there + codeArray(pc).asInstanceOf[ByteCode].stackEffect.get != 0)
            throw CodeFreezingException("Stack not empty after return.\n" + stackTrace)
        }
        case bc: ByteCode if !bc.stackEffect.isEmpty         => setHeight(from + bc.length.get, there + bc.stackEffect.get)
        case GETFIELD                                        => codeArray(pc + 1) match {
          case RawBytes(idx) => setHeight(from + 3, (there + constantPool.getFieldSize(idx)) - 1)
          case _             => throw CodeFreezingException("Expected RawBytes after GETFIELD.")
        }
        case GETSTATIC                                       => codeArray(pc + 1) match {
          case RawBytes(idx) => setHeight(from + 3, there + constantPool.getFieldSize(idx))
          case _             => throw CodeFreezingException("Expected RawBytes after GETSTATIC.")
        }
        case PUTFIELD                                        => codeArray(pc + 1) match {
          case RawBytes(idx) => setHeight(from + 3, there - constantPool.getFieldSize(idx) - 1)
          case _             => throw CodeFreezingException("Expected RawBytes after PUTFIELD.")
        }
        case PUTSTATIC                                       => codeArray(pc + 1) match {
          case RawBytes(idx) => // setHeight(from + 3, -(there + constantPool.getFieldSize(idx)))
            setHeight(from + 3, there - constantPool.getFieldSize(idx))
          case _             => throw CodeFreezingException("Expected RawBytes after PUTSTATIC.")
        }
        case INVOKEVIRTUAL | INVOKESPECIAL                   => codeArray(pc + 1) match {
          case RawBytes(idx) => {
            val se = constantPool.getMethodEffect(idx) - 1
            setHeight(from + 3, there + se)
          }
          case _             => throw CodeFreezingException("Expected RawBytes after INVOKEVIRTUAL/INVOKESPECIAL.")
        }
        case INVOKEINTERFACE                                 => codeArray(pc + 1) match {
          case RawBytes(idx) => codeArray(pc + 3) match {
            case RawByte(n) => {
              val se = constantPool.getMethodEffect(idx)
              codeArray(pc + 4) match {
                case RawByte(0) => setHeight(from + 5, there + se - 1)
                case _          => throw CodeFreezingException("Expected RawByte(0) as the last param for INVOKEINTERFACE")
              }
            }
            case b          => throw CodeFreezingException("Expected RawByte after the RawBytes in INVOKEINTERFACE @ " + pc + " ; found: " + b)

          }
          case _             => throw CodeFreezingException("Expected RawBytes after INVOKEINTERFACE.")
        }
        case INVOKESTATIC                                    => codeArray(pc + 1) match {
          case RawBytes(idx) => {
            val se = constantPool.getMethodEffect(idx)
            setHeight(from + 3, there + se)
          }
          case _             => throw CodeFreezingException("Expected RawBytes after INVOKESTATIC.")
        }
        case MULTIANEWARRAY                                  => codeArray(pc + 1) match {
          case RawBytes(idx) => codeArray(pc + 3) match {
            case RawByte(dimension) => setHeight(from + 4, there - dimension + 1)
            case _                  => throw CodeFreezingException("Expected RawByte after the RawBytes in MULTINEWARRAY.")
          }
          case _             => throw CodeFreezingException("Expected RawBytes after MULTINEWARRAY.")

        }
        case g@Goto(_)                                       => setHeight(from + g.offset, there)
        case co: ControlOperator                             => {
          setHeight(from + co.offset, there + co.opCode.stackEffect.get)
          setHeight(from + co.opCode.length.get, there + co.opCode.stackEffect.get)
        }
        case other@_                                         => sys.error("Computation of stack height unsupported for " + other)
      }
    }

    setHeight(0, 0)
    println(stackTrace)
    heightArray.max.asInstanceOf[U2]
  }

  def print: Unit = if (!frozen) {
    var pc = 0
    for (abc <- abcBuffer) {
      abc match {
        case Label(name) =>
          println(name + ":")
        case _           =>
          println("%5d %s".format(pc, abc))
          pc += abc.size
      }
    }
  }
}
