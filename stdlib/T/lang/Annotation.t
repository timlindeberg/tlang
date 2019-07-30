package T::lang

/**
 * Base trait for Annotation classes in tlang. Classes that implement the
 * T::lang::Annotation trait can be used to annotate methods and classes. Such
 * annotations will be visible to the tlang compiler appear in the generated
 * class files so they can be found using runtime reflection.
 */
trait Annotation
