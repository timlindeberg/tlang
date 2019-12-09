package tlang
package compiler
package imports

import org.apache.bcel.classfile.{ClassParser, JavaClass}

trait ClassFile {
  def path: String
}
trait JavaClassFile extends ClassFile {

  def parse: JavaClass = parser.parse()
  protected def parser: ClassParser
}

case class JarClassFile(path: String, className: String) extends JavaClassFile {
  override protected def parser: ClassParser = new ClassParser(path, className)
}
case class RegularClassFile(path: String) extends JavaClassFile {
  override protected def parser: ClassParser = new ClassParser(path)
}
case class TemplateFile(path: String) extends ClassFile
