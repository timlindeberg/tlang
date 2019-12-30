package cafebabe

import cafebabe.ClassFileTypes._

/** A method handler is used to attach attributes to a method. In particular,
 * it can return an associated <code>CodeHandler</code> which can be used to
 * specify a method's body. <code>MethodHandler</code>s should not be created
 * manually but rather obtained directly when adding a method to a
 * <code>ClassFile</code>. */
class MethodHandler private[cafebabe](
  m: MethodInfo,
  c: Option[CodeAttributeInfo],
  cp: ConstantPool,
  paramTypes: List[String],
  signature: String) extends Annotatable {
  private var ch: Option[CodeHandler] = None

  private lazy val annotationNameIndex = cp.addString(Annotatable.ClassPoolName)
  private lazy val parameterAnnotationNameIndex = cp.addString("RuntimeVisibleParameterAnnotations")

  def codeHandler: CodeHandler = {
    if (c.isEmpty)
      sys.error("Can't get a code handler from an abstract method.")

    if (ch.isEmpty)
      ch = Some(new CodeHandler(c.get, cp, paramTypes.mkString, m.isStatic, signature))

    ch.get
  }

  def addParameterAnnotation(index: Int, name: String): AnnotationHandler = {
    val annotationAttribute = m.getParameterAnnotationAttribute(parameterAnnotationNameIndex, paramTypes.size)
    val annotationInfo = new AnnotationInfo(cp.addString(name), Nil)
    annotationAttribute.addAnnotation(index, annotationInfo)
    new AnnotationHandler(annotationInfo, cp)
  }

  override def addAnnotation(name: String): AnnotationHandler = {
    addAnnotation(name, m.getAnnotationAttribute(annotationNameIndex), cp)
  }

  def setFlags(flags: U2): Unit = {
    if (ch.isDefined) {
      if (m.isStatic != ((flags & Flags.METHOD_ACC_STATIC) != 0)) {
        sys.error("Cannot change the `static` attribute of a method after its CodeHandler has been issued.")
      }
    }
    m.accessFlags = flags
  }
}
