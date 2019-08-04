package cafebabe

trait Annotatable {
  def addAnnotation(name: String): AnnotationHandler
}
