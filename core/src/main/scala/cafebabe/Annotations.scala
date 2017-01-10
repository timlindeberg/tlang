package cafebabe

import cafebabe.ClassFileTypes.{U2, U4}

/**
  * Created by Tim Lindeberg on 8/21/2016.
  */

class RuntimeAnnotationAttribute(override val attributeNameIndex: U2) extends AttributeInfo(attributeNameIndex, Nil) {

  var annotations: List[AnnotationInfo] = Nil

  override def toStream(stream: ByteStream): ByteStream = {
    val numAnnotations: U2 = annotations.size.asInstanceOf[U2]
    val attributeLength: U4 = 4 * numAnnotations + 2
    stream << attributeNameIndex << attributeLength << numAnnotations << annotations
  }

}

// These can also contain ElementValuePairs but for now they can only use the typeIndex
class AnnotationInfo(typeIndex: U2) extends Streamable {
  override def toStream(stream: ByteStream): ByteStream =
    stream << typeIndex << 0.asInstanceOf[U2]
}
