package cafebabe

object Annotatable {
  val ClassPoolName = "RuntimeInvisibleAnnotations"
}

trait Annotatable {
  def addAnnotation(name: String): AnnotationHandler
  protected def addAnnotation(name: String, annotationAttribute: AnnotationAttribute, cp: ConstantPool): AnnotationHandler = {
    val annotationInfo = new AnnotationInfo(cp.addString(name), Nil)
    annotationAttribute.addAnnotation(annotationInfo)
    new AnnotationHandler(annotationInfo, cp)
  }
}
