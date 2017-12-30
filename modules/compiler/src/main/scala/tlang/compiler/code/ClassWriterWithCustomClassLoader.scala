package tlang.compiler.code

import org.objectweb.asm.ClassWriter

class ClassWriterWithCustomClassLoader(classLoader: ClassLoader, flags: Int) extends ClassWriter(flags) {

  // In some cases BCEL can need access to the generated classes when generating
  // stack map frames. We use a new class loader with access to the newly
  // generated classes.

  override def getCommonSuperClass(type1: String, type2: String): String = {
    var c: Class[_] = null
    var d: Class[_] = null
    try {
      c = Class.forName(type1.replace('/', '.'), false, classLoader)
      d = Class.forName(type2.replace('/', '.'), false, classLoader)
    } catch {
      case e: Exception =>
        throw new RuntimeException(e.toString)
    }

    if (c.isAssignableFrom(d)) return type1
    if (d.isAssignableFrom(c)) return type2
    if (c.isInterface || d.isInterface) return "java/lang/Object"

    do c = c.getSuperclass while ( { !c.isAssignableFrom(d) })
    c.getName.replace('.', '/')
  }

}
