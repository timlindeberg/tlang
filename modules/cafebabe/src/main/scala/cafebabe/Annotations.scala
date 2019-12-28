package cafebabe

import cafebabe.ClassFileTypes.{U1, U2, U4}

import scala.collection.mutable.ListBuffer

class AnnotationAttribute(override val attributeNameIndex: U2) extends AttributeInfo(attributeNameIndex, Nil) {
  private var annotations: List[AnnotationInfo] = Nil

  def addAnnotation(annotation: AnnotationInfo): Unit = {
    annotations ::= annotation
  }

  override def toStream(stream: ByteStream): ByteStream = {
    val numAnnotations: U2 = annotations.size.asInstanceOf[U2]
    val attributeLength: U4 = 2 + annotations.map { _.size }.sum
    stream << attributeNameIndex << attributeLength << numAnnotations << annotations
  }
}

class ParameterAnnotationAttribute(override val attributeNameIndex: U2, val numParameters: U1) extends AttributeInfo(attributeNameIndex, Nil) {
  private val parameterAnnotations: List[ListBuffer[AnnotationInfo]] = List.fill(numParameters)(ListBuffer())

  def addAnnotation(index: Int, annotation: AnnotationInfo): Unit = {
    parameterAnnotations(index) += annotation
  }

  override def toStream(stream: ByteStream): ByteStream = {
    val attributeLength: U4 = 1 + parameterAnnotations
      .map { annotations => 2 + annotations.map { _.size }.sum }
      .sum
    stream << attributeNameIndex << attributeLength << numParameters

    parameterAnnotations foreach { annotations =>
      stream << annotations.size.asInstanceOf[U2] << annotations
    }
    stream
  }
}

class AnnotationInfo(typeIndex: U2, var elementValuePairs: List[AnnotationElementValue]) extends Streamable {

  def size: U4 = 4 + elementValuePairs.size * 5

  override def toStream(stream: ByteStream): ByteStream =
    stream <<
      typeIndex <<
      elementValuePairs.size.asInstanceOf[U2] <<
      elementValuePairs
}

// Currently only supports constants (const_value_index in the JVM spec)
class AnnotationElementValue(tag: U1, nameIndex: U2, valueIndex: U2) extends Streamable {
  override def toStream(stream: ByteStream): ByteStream =
    stream << nameIndex << tag << valueIndex
}
