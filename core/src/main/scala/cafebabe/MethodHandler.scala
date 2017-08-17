package cafebabe

import cafebabe.ClassFileTypes._

/** A method handler is used to attach attributes to a method. In particular,
  * it can return an associated <validtests.code>CodeHandler</validtests.code> which can be used to
  * specify a method's body. <validtests.code>MethodHandler</validtests.code>s should not be created
  * manually but rather obtained directly when adding a method to a
  * <validtests.code>ClassFile</validtests.code>. */
class MethodHandler private[cafebabe](m: MethodInfo, c: CodeAttributeInfo, cp: ConstantPool, paramTypes: String, signature: String) {
  private var ch: Option[CodeHandler] = None

  private lazy val annotationNameIndex = cp.addString("RuntimeInvisibleAnnotations")

  def codeHandler: CodeHandler = {
    if (ch.isEmpty)
      ch = Some(new CodeHandler(c, cp, paramTypes, m.isStatic, signature))

    ch.get
  }

  def addAnnotation(name: String) = {
    m.getAnnotationAttribute(annotationNameIndex).annotations ::= new AnnotationInfo(cp.addString(name))
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
