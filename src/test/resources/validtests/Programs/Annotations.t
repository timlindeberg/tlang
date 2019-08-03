trait AnnotationA : Annotation
trait AnnotationB : Annotation =
	Def S(): String

trait AnnotationC : Annotation =
	Def A(): Int
	Def B(): Double
	Def C(): String

@AnnotationA
class A

val e = new A()
val c = e.getClass()
val annotations = c.getAnnotations()
println(annotations)
