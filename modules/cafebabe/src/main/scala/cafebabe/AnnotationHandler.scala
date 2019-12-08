package cafebabe

import cafebabe.ClassFileTypes.{U1, U2}


/** A method handler is used to attach attributes to a method. In particular,
 * it can return an associated <code>CodeHandler</code> which can be used to
 * specify a method's body. <code>MethodHandler</code>s should not be created
 * manually but rather obtained directly when adding a method to a
 * <code>ClassFile</code>. */
class AnnotationHandler private[cafebabe](annotationInfo: AnnotationInfo, cp: ConstantPool) {

  def addValue(name: String, value: Int): Unit = addValue('I', cp.addString(name), cp.addInt(value))
  def addValue(name: String, value: Long): Unit = addValue('J', cp.addString(name), cp.addLong(value))
  def addValue(name: String, value: Float): Unit = addValue('F', cp.addString(name), cp.addFloat(value))
  def addValue(name: String, value: Double): Unit = addValue('D', cp.addString(name), cp.addDouble(value))
  def addValue(name: String, value: String): Unit = addValue('s', cp.addString(name), cp.addString(value))

  private def addValue(tag: Char, nameIndex: U2, valueIndex: U2): Unit = {
    annotationInfo.elementValuePairs ::= new AnnotationElementValue(tag.toInt.asInstanceOf[U1], nameIndex, valueIndex)
  }

}
