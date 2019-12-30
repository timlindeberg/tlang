package cafebabe

import cafebabe.ClassFileTypes._

/** A field handler is used to attach attributes to a field (currently, only
 * flags). <code>FieldHandler</code>s should not be created manually but
 * rather obtained directly when adding a field method to a
 * <code>ClassFile</code>. */
class FieldHandler private[cafebabe](f: FieldInfo, cp: ConstantPool) extends Annotatable {

  private lazy val annotationNameIndex = cp.addString(Annotatable.ClassPoolName)

  def setFlags(flags: U2): Unit = { f.accessFlags = flags }

  override def addAnnotation(name: String): AnnotationHandler = {
    addAnnotation(name, f.getAnnotationAttribute(annotationNameIndex), cp)
  }
}
