package cafebabe

import cafebabe.ClassFileTypes._

case class MethodInfo(var accessFlags: U2, nameIndex: U2, descriptorIndex: U2, var attributes: List[AttributeInfo]) extends Streamable {

  var annotationAttribute: Option[RuntimeAnnotationAttribute] = None

  override def toStream(stream: ByteStream): ByteStream = {
    stream << accessFlags << nameIndex << descriptorIndex
    stream << attributes.size.asInstanceOf[U2] << attributes
  }

  def getAnnotationAttribute(attributeNameIndex: U2) = {
    if (annotationAttribute.isEmpty) {
      val attr = new RuntimeAnnotationAttribute(attributeNameIndex)
      annotationAttribute = Some(attr)
      attributes ::= attr
    }
    annotationAttribute.get
  }

  def addAttribute(attributeInfo: AttributeInfo) = attributes ::= attributeInfo
  def hasAttribute(attributeInfo: AttributeInfo) = attributes.contains(attributeInfo)

  def isStatic: Boolean = (accessFlags & Flags.METHOD_ACC_STATIC) != 0
}
