package cafebabe

import cafebabe.AbstractByteCodes._
import cafebabe.ByteCodes._
import tlang.formatting.Colors.Color
import tlang.formatting.Formatting

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

case class CodegenerationStackTrace(
  abcBuffer: ListBuffer[AbstractByteCode],
  heightArray: Array[Int],
  cp: ConstantPool,
  signature: String,
  formatting: Formatting
) {

  val heights: Array[Int]             = heightArray.clone()
  val abcs   : List[AbstractByteCode] = abcBuffer.toList

  import formatting._

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

  private def getLabelColor(label: String) = {
    colorMap.getOrElseUpdate(label, {
      colorIndex = (colorIndex + 1) % FGColors.length
      FGColors(colorIndex)
    })
  }

  private def labelColor(label: String): String = {
    val color = getLabelColor(label)
    color(label)
  }

  private type StackTraceLine = (String, String, String, String, String)

  override def toString: String = content.mkString
  def content: List[StackTraceLine] = {
    if (abcs.isEmpty)
      return Nil

    val lines: ListBuffer[StackTraceLine] = ListBuffer()
    var pc = 0
    var currentLineNumber = 0

    def appendLine(abc: AbstractByteCode, extraInfo: String) = {
      val h = if (pc > heights.length) UninitializedHeight else heights(pc)
      val height = if (h == UninitializedHeight) "" else String.valueOf(h)
      val line = (NumColor(currentLineNumber), KeywordColor(pc), NumColor(height), KeywordColor(abc), extraInfo)
      lines += line
    }

    var i = 0
    while (i < abcs.size) {
      val abc = abcs(i)
      abc match {
        case Label(name)                                =>
          import formatting.Horizontal
          val color = getLabelColor(name)
          // Reasonable estimates for the sizes of the columns (mostly based on the width of the header)
          val line = (color(Horizontal * 4), color(Horizontal * 3), color(Horizontal * 6), color(Horizontal * 13), color(Horizontal * 2 + "> " + name))
          lines += line
        case LineNumber(line)                           =>
          currentLineNumber = line
        case _: RawByte | _: RawBytes                   =>
        case NEWARRAY                                   =>
          abcs(i + 1) match {
            case RawByte(tpe) => appendLine(abc, NumColor(types(tpe)))
          }
        case IINC                                       =>
          abcs(i + 1) match {
            case RawByte(index) => abcs(i + 2) match {
              case RawByte(amount) => appendLine(abc, NumColor(s"$index $amount"))
            }
            // Can use multiple bytes if preceded by a WIDE
            case RawBytes(index) => abcs(i + 2) match {
              case RawBytes(amount) => appendLine(abc, NumColor(s"$index $amount"))
            }
          }
        case BIPUSH | SIPUSH | ALOAD | ILOAD | FLOAD | LLOAD | DLOAD |
             ASTORE | ISTORE | FSTORE | LSTORE | DSTORE =>
          abcs(i + 1) match {
            case RawByte(value)  => appendLine(abc, NumColor(value))
            case RawBytes(value) => appendLine(abc, NumColor(value))
          }
        case co: ControlOperator                        =>
          appendLine(co.opCode, labelColor(co.target.trim))
        case _                                          =>
          val extraInfo = if (i + 1 < abcs.size) {
            abcs(i + 1) match {
              case RawByte(idx)  => cp.getByteInfo(idx, formatting)
              case RawBytes(idx) => cp.getByteInfo(idx, formatting)
              case _             => ""
            }
          } else ""
          appendLine(abc, extraInfo)
      }
      pc += abc.size
      i += 1
    }
    lines.toList
  }

  def header: String = {
    val split1 = signature.split("\\.")
    val className = split1(0)
    val rest = split1(1)
    Green(className) + "." + Bold(Magenta(rest))
  }

}
