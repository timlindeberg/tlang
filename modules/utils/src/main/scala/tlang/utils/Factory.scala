package tlang
package utils


abstract class Factory[Type](dependencies: Any*) {
//
//  private val cache: mutable.Map[Class[_], ConstructorWithArguments[_]] = mutable.Map()
//
//  def create[T <: Type : ClassTag]: T = create()
//
//  def create[T <: Type : ClassTag](extraArguments: Any*): T = {
//    val clazz = classTag[T].runtimeClass
//    val cons = cache.getOrElseUpdate(clazz, findConstructor[T](extraArguments))
//    cons.asInstanceOf[ConstructorWithArguments[T]]()
//  }
//
//  private def findConstructor[T <: Type : ClassTag](extraArguments: Any*): ConstructorWithArguments[T] = {
//    val clazz = classTag[T].runtimeClass
//    clazz.getConstructors
//      .map { cons => ConstructorWithArguments(cons.asInstanceOf[Constructor[T]], extraArguments) }
//      .find { _.hasMatchingDependencies }
//      .getOrElse { ErrorCouldNotResolveArguments(clazz) }
//  }
//
//  private def ErrorCouldNotResolveArguments(tClass: Class[_]): Nothing = {
//    throw new RuntimeException(s"Could not create class of type ${ tClass.getSimpleName }")
//  }
//
//  private case class ConstructorWithArguments[T](constructor: Constructor[T], extraArguments: Any*) {
//
//    private val args: Array[Any] = resolveArguments()
//
//    def hasMatchingDependencies: Boolean = !args.contains(null)
//
//    def apply(): T = constructor.newInstance(args: _*)
//
//    private def resolveArguments(): Array[Any] = {
//      val types = constructor.getParameterTypes
//      val resolvedDependencies = types.take(constructor.getParameterCount - extraArguments.size) map {
//        findDependency(_).orNull
//      }
//      resolvedDependencies ++ extraArguments
//    }
//
//    private def findDependency(tpe: Class[_]): Option[Any] =
//      dependencies.find(v => tpe.isAssignableFrom(v.getClass))
//
//  }


}
