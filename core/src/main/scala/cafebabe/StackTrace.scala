package cafebabe

import cafebabe.AbstractByteCodes._
import cafebabe.ByteCodes._
import tcompiler.utils.Colorizer

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
  * Created by Tim Lindeberg on 2/2/2017.
  */
case class StackTrace(abcBuffer: ListBuffer[AbstractByteCode], heightArray: Array[Int], cp: ConstantPool, signature: String) {

  private val UninitializedHeight: Int = Int.MinValue
  private val types                    = Map(
    4 -> "T_BOOLEAN",
    5 -> "T_CHAR",
    6 -> "T_FLOAT",
    7 -> "T_DOUBLE",
    8 -> "T_BYTE",
    9 -> "T_SHORT",
    10 -> "T_INT",
    11 -> "T_LONG"
  )

  private var colorIndex = -1
  private val colorMap   = mutable.HashMap[String, String]()
  private def labelColor(label: String, colorizer: Colorizer) = {
    import colorizer._
    val color = colorMap.getOrElseUpdate(label, {
      colorIndex = (colorIndex + 1) % Colors.length
      Colors(colorIndex)
    })
    color + label + Reset
  }


  def apply(colorizer: Colorizer): String = {
    import colorizer._

    if (abcBuffer.isEmpty)
      return ""

    val sb = new StringBuilder()
    sb ++= header(colorizer, signature)
    sb ++= s"$Bold%-6s %-6s %-6s %-15s %s$Reset\n".format("Line", "PC", "Height", "ByteCode", "Info")

    var pc = 0
    var currentLineNumber = 0

    def appendAbc(abc: AbstractByteCode, extraInfo: String) = {
      val h = if (pc > heightArray.length) UninitializedHeight else heightArray(pc)
      val height = if (h == UninitializedHeight) "" else String.valueOf(h)
      // Colorize labels
      sb ++= s"$VarColor%-6d $KeywordColor%-6d $VarColor%-6s $KeywordColor%-15s %s$Reset\n"
        .format(currentLineNumber, pc, height, abc, extraInfo)
    }

    var i = 0
    while (i < abcBuffer.size) {
      val abc = abcBuffer(i)
      abc match {
        case Label(name)                                =>
          sb.append(s"%16s%s:\n".format("", labelColor(name, colorizer)))
        case LineNumber(line)                           =>
          currentLineNumber = line
        case _: RawByte | _: RawBytes                   =>
        case NEWARRAY                                   =>
          abcBuffer(i + 1) match {
            case RawByte(tpe) => appendAbc(abc, s"$NumColor${types(tpe)}")
          }
        case IINC                                       =>
          abcBuffer(i + 1) match {
            case RawByte(index) => abcBuffer(i + 2) match {
              case RawByte(amount) => appendAbc(abc, s"$NumColor$index $amount")
            }
          }
        case BIPUSH | SIPUSH | ALOAD | ILOAD | FLOAD | LLOAD | DLOAD |
             ASTORE | ISTORE | FSTORE | LSTORE | DSTORE =>
          abcBuffer(i + 1) match {
            case RawByte(value)  => appendAbc(abc, s"$NumColor$value")
            case RawBytes(value) => appendAbc(abc, s"$NumColor$value")
          }
        case co: ControlOperator                        =>
          appendAbc(co.opCode, labelColor(co.target, colorizer))
        case _                                          =>
          var extraInfo = ""
          if (i + 1 < abcBuffer.size) {
            abcBuffer(i + 1) match {
              case RawByte(idx)  => extraInfo = cp.getByteInfo(idx, colorizer)
              case RawBytes(idx) => extraInfo = cp.getByteInfo(idx, colorizer)
              case _             =>
            }
          }
          appendAbc(abc, extraInfo)
      }
      pc += abc.size
      i += 1
    }
    sb ++= "\n"
    sb.toString
  }

  private def header(colorizer: Colorizer, signature: String) = {
    import colorizer._
    val split1 = signature.split("\\.")
    val className = split1(0)
    val split2 = split1(1).split(":")
    val methName = split2(0)
    val sig = split2(1)
    s"${Bold("[>")} ${ClassColor(className)}.${MethodColor(methName)}: ${ClassColor(sig)} ${Bold("<]")}\n"
  }

}
