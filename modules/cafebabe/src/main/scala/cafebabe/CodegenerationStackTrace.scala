package cafebabe

import cafebabe.AbstractByteCodes._
import cafebabe.ByteCodes._
import tlang.formatting.Colors.Color
import tlang.formatting.Formatter
import tlang.formatting.grid.{CenteredContent, Divider}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

case class CodegenerationStackTrace(
  abcBuffer: ListBuffer[AbstractByteCode],
  heightArray: Array[Int],
  cp: ConstantPool,
  signature: String,
)(
  implicit formatter: Formatter
) {

  val heights: Array[Int] = heightArray.clone()
  val abcs: IndexedSeq[AbstractByteCode] = abcBuffer.toIndexedSeq

  import formatter._

  private val UninitializedHeight: Int = Int.MinValue
  private val types = Map(
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
  private val colorMap = mutable.HashMap[String, Color]()

  private def getLabelColor(label: String) = {
    colorMap.getOrElseUpdate(label, {
      colorIndex = (colorIndex + 1) % AllColors.length
      AllColors(colorIndex)
    })
  }

  private def labelColor(label: String): String = {
    val color = getLabelColor(label)
    color(label)
  }

  // Any since we can return either a Divider or a String
  private type StackTraceLine = (Any, Any, Any, Any, Any)

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
          val color = getLabelColor(name)
          val divider = Divider(Horizontal, color)
          val line = (divider, divider, divider, divider, CenteredContent(s" $name ", color, Horizontal))
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
              case RawByte(idx)  => cp.getByteInfo(idx)
              case RawBytes(idx) => cp.getByteInfo(idx)
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

  private val Header = """(.+?)\.(.+?)<\->(.*)""".r
  def header: String = {
    signature match {
      case Header(className, methodSignature, methodJVMSignature) =>
        Green(className) + "." + Bold(Magenta(methodSignature)) + " " + Bold(Blue(methodJVMSignature))
      case _                                                      => signature
    }
  }
}
