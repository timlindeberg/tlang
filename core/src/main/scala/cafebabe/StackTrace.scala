package cafebabe

import cafebabe.AbstractByteCodes._
import cafebabe.ByteCodes._
import tlang.compiler.error.Formatting
import tlang.utils.Colors
import tlang.utils.Colors.Color

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
  * Created by Tim Lindeberg on 2/2/2017.
  */
case class StackTrace(
  abcBuffer: ListBuffer[AbstractByteCode],
  heightArray: Array[Int],
  cp: ConstantPool,
  signature: String,
  formatting: Formatting
) {

  val heights: Array[Int]             = heightArray.clone()
  val abcs   : List[AbstractByteCode] = abcBuffer.toList

  import formatting._
  import formatting.colors._

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
  private val colorMap   = mutable.HashMap[String, Color]()
  private def labelColor(label: String, colors: Colors) = {
    val color = colorMap.getOrElseUpdate(label, {
      colorIndex = (colorIndex + 1) % AllColors.length
      AllColors(colorIndex)
    })
    color(label)
  }

  override def toString: String = content
  def content: String = makeStacktrace

  private def makeStacktrace: String = {
    if (abcs.isEmpty)
      return ""

    val sb = new StringBuilder()
    var pc = 0
    var currentLineNumber = 0

    var first = true
    def appendLine(abc: AbstractByteCode, extraInfo: String) = {
      val h = if (pc > heights.length) UninitializedHeight else heights(pc)
      val height = if (h == UninitializedHeight) "" else String.valueOf(h)
      // Colorize labels
      if (!first)
        sb ++= "\n"
      first = false
      sb ++= s"$NumColor%-6s $KeywordColor%-6s $NumColor%-7s $KeywordColor%-15s%s$Reset"
        .format(currentLineNumber, pc, height, abc, extraInfo)
    }

    var i = 0
    while (i < abcs.size) {
      val abc = abcs(i)
      abc match {
        case Label(name)                                =>
          val label = labelColor(name, colors)
          sb.append("\n" + rightAlign(label))
        case LineNumber(line)                           =>
          currentLineNumber = line
        case _: RawByte | _: RawBytes                   =>
        case NEWARRAY                                   =>
          abcs(i + 1) match {
            case RawByte(tpe) => appendLine(abc, s"$NumColor${types(tpe)}")
          }
        case IINC                                       =>
          abcs(i + 1) match {
            case RawByte(index) => abcs(i + 2) match {
              case RawByte(amount) => appendLine(abc, s"$NumColor$index $amount")
            }
            // Can use multiple bytes if preceded by a WIDE
            case RawBytes(index) => abcs(i + 2) match {
              case RawBytes(amount) => appendLine(abc, s"$NumColor$index $amount")
            }
          }
        case BIPUSH | SIPUSH | ALOAD | ILOAD | FLOAD | LLOAD | DLOAD |
             ASTORE | ISTORE | FSTORE | LSTORE | DSTORE =>
          abcs(i + 1) match {
            case RawByte(value)  => appendLine(abc, s"$NumColor$value")
            case RawBytes(value) => appendLine(abc, s"$NumColor$value")
          }
        case co: ControlOperator                        =>
          appendLine(co.opCode, labelColor(co.target.trim, colors))
        case _                                          =>
          val extraInfo = if (i + 1 < abcs.size) {
            abcs(i + 1) match {
              case RawByte(idx)  => cp.getByteInfo(idx, colors)
              case RawBytes(idx) => cp.getByteInfo(idx, colors)
              case _             => ""
            }
          } else ""
          appendLine(abc, extraInfo)
      }
      pc += abc.size
      i += 1
    }
    sb.toString
  }

  def header: String = {
    val split1 = signature.split("\\.")
    val className = split1(0)
    val split2 = split1(1).split(":")
    val methName = split2(0)
    val sig = split2(1)
    Magenta(className) + "." + Bold(Magenta(methName)) + ":" + sig
  }

}
