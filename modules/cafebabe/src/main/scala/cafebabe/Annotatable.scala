package cafebabe

object Annotatable {
  val ClassPoolName = "RuntimeInvisibleAnnotations"
}

trait Annotatable {
  def addAnnotation(name: String): AnnotationHandler
}
