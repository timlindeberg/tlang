package cafebabe

import cafebabe.ClassFileTypes._

class StackMapTableAttribute(val nameIndex: U2, stackMapFrames: List[StackMapFrame]) extends AttributeInfo(nameIndex, Nil) {

  override def toStream(stream: ByteStream): ByteStream = {
    val stackMapSize: U4 = 2 + stackMapFrames.foldLeft(0)((sum, stackMap) => sum + stackMap.size)
    val ne: U2 = stackMapFrames.size

    stream << nameIndex << stackMapSize << ne
    stackMapFrames.foreach(stream << _)
    stream
  }
}

object VerificationTypeInfoTags {
  val Top               = 0
  val Integer           = 1
  val Float             = 2
  val Double            = 3
  val Long              = 4
  val Null              = 5
  val UninitializedThis = 6
  val Object            = 7
  val Uninitialized     = 8

  def size(a: Array[VerificationTypeInfo]): U4 = a.foldLeft(0)((sum, typeInfo) => sum + typeInfo.size)
}

object StackMapFrameTypes {
  val SameFrame                         = 0 to 63
  val SameLocals1StackItemFrame         = 64 to 127
  val SameLocals1StackItemFrameExtended = 247 to 247
  val ChopFrame                         = 248 to 250
  val SameFrameExtended                 = 251 to 251
  val AppendFrame                       = 252 to 254
  val FullFrame                         = 255 to 255
}

import cafebabe.VerificationTypeInfoTags._

abstract class VerificationTypeInfo(tag: U1, val size: U4) extends Streamable {
  override def toStream(stream: ByteStream): ByteStream = stream << tag
}

case class TopVariableInfo() extends VerificationTypeInfo(Top, 1)
case class IntegerVariableInfo() extends VerificationTypeInfo(Integer, 1)
case class FloatVariableInfo() extends VerificationTypeInfo(Float, 1)
case class DoubleVariableInfo() extends VerificationTypeInfo(Double, 1)
case class LongVariableInfo() extends VerificationTypeInfo(Long, 1)
case class NullVariableInfo() extends VerificationTypeInfo(Null, 1)
case class UninitializedThisVariableInfo() extends VerificationTypeInfo(UninitializedThis, 1)
case class ObjectVariableInfo(constantPoolIndex: U2) extends VerificationTypeInfo(Object, 3) {
  override def toStream(stream: ByteStream): ByteStream = super.toStream(stream) << constantPoolIndex
}
case class UninitializedVariableInfo() extends VerificationTypeInfo(Uninitialized, 3) {
  val offset: Option[U2] = None

  override def toStream(stream: ByteStream): ByteStream = offset match {
    case Some(offset) => super.toStream(stream) << offset
    case None         => sys.error("Uninitialized variable info needs an offset!")
  }
}


abstract class StackMapFrame(frameType: U1) extends Streamable {
  override def toStream(stream: ByteStream): ByteStream = stream << frameType
  val size: U4
}

case class SameFrame(frameType: U1) extends StackMapFrame(frameType) {
  assert(StackMapFrameTypes.SameFrame.contains(frameType))
  val size = 1
}

case class SameLocals1StackItemFrame(frameType: U1, stack: Array[VerificationTypeInfo]) extends StackMapFrame(frameType) {
  assert(StackMapFrameTypes.SameLocals1StackItemFrame.contains(frameType))
  assert(stack.length == 2)
  override def toStream(stream: ByteStream): ByteStream = {
    super.toStream(stream)
    stack.foreach(stream << _)
    stream
  }
  val size = 1 + VerificationTypeInfoTags.size(stack)

}

case class SameLocals1StackItemFrameExtended(frameType: U1, offsetDelta: U2, stack: Array[VerificationTypeInfo]) extends StackMapFrame(frameType) {
  assert(StackMapFrameTypes.SameLocals1StackItemFrameExtended.contains(frameType))
  assert(stack.length == 2)
  override def toStream(stream: ByteStream): ByteStream = {
    super.toStream(stream) << offsetDelta
    stack.foreach(stream << _)
    stream
  }
  val size = 3 + VerificationTypeInfoTags.size(stack)
}

case class ChopFrame(frameType: U1, offsetDelta: U2) extends StackMapFrame(frameType) {
  assert(StackMapFrameTypes.ChopFrame.contains(frameType))
  override def toStream(stream: ByteStream): ByteStream = super.toStream(stream) << offsetDelta
  val size = 3
}

case class SameFrameExtended(frameType: U1, offsetDelta: U2) extends StackMapFrame(frameType) {
  assert(StackMapFrameTypes.SameFrameExtended.contains(frameType))
  override def toStream(stream: ByteStream): ByteStream = super.toStream(stream) << offsetDelta
  val size = 3

}

case class AppendFrame(frameType: U1, offsetDelta: U2, locals: Array[VerificationTypeInfo]) extends StackMapFrame(frameType) {
  assert(StackMapFrameTypes.AppendFrame.contains(frameType))
  assert(locals.length == frameType - 251)
  override def toStream(stream: ByteStream): ByteStream = {
    super.toStream(stream) << offsetDelta
    locals.foreach(stream << _)
    stream
  }
  val size = 3 + VerificationTypeInfoTags.size(locals)
}

case class FullFrame(offsetDelta: U2,
  numberOfLocals: U2,
  locals: Array[VerificationTypeInfo],
  numberOfStackItems: U2,
  stack: Array[VerificationTypeInfo]
) extends StackMapFrame(255) {
  assert(locals.length == numberOfLocals)
  assert(stack.length == numberOfStackItems)

  override def toStream(stream: ByteStream): ByteStream = {
    super.toStream(stream) << offsetDelta << numberOfLocals
    locals.foreach(stream << _)
    stream << numberOfStackItems
    stack.foreach(stream << _)
    stream
  }
  val size = 7 + VerificationTypeInfoTags.size(locals) + VerificationTypeInfoTags.size(stack)

}