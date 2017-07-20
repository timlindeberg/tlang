package tlang.compiler.imports

import java.io.File
import java.nio.file.{Files, Path}
import java.util.jar.JarFile

import scala.collection.mutable.ArrayBuffer
import scala.util.Sorting

class ClassPathParser(paths: Set[String]) {

  private val (jars, dirs) = paths.partition(_.endsWith(".jar"))

  private val estimatedNumClasses = jars.map(_.length).sum + dirs.size * 50

  private val classes   : ArrayBuffer[String]                  = new ArrayBuffer(estimatedNumClasses)
  private val pathToFile: java.util.HashMap[String, ClassFile] = new java.util.HashMap(estimatedNumClasses)

  def parse(): (Map[String, ClassFile], Array[String]) = {
    jars foreach { path => addClasses(new JarFile(path)) }
    dirs foreach { path => addClasses(new File(path)) }

    val classArray = classes.toArray
    Sorting.quickSort(classArray)

    (toImmutableMap(pathToFile), classArray)
  }

  private def addClasses(jar: JarFile): Unit = {
    jar.stream()
      .filter(_.getName.endsWith(".class"))
      .forEach { entry =>
        val entryName = entry.getName
        val name = entryName.substring(0, entryName.length - ".class".length)
        addClass(name, JarClassFile(jar.getName, entryName))
      }
    // TODO: Template files should also be able to reside in jar-files
  }

  private def addClasses(dir: File): Unit = {
    val classPath = dir.getAbsolutePath
    def addFile(p: Path, fileType: String, classFileType: String => ClassFile) = {
      val path = p.toString
      val name = path.substring(classPath.length + 1, path.length - fileType.length).replaceAll("\\\\", "/")
      addClass(name, classFileType(path))
    }
    Files.walk(dir.toPath, 500)
      .forEach { path =>
        path.getFileName.toString match {
          case f if f.endsWith(".class") => addFile(path, ".class", RegularClassFile)
          case f if f.endsWith(".t")     => addFile(path, ".t", TemplateFile)
          case _                         =>
        }
      }
  }

  private def addClass(name: String, classFile: ClassFile) = {
    classes += name
    pathToFile.get(name) match {
      case _: TemplateFile => pathToFile.put(name, classFile) // Class files has priority over template files
      case null            => pathToFile.put(name, classFile)
      case _               =>
    }
  }

  private def toImmutableMap[K, V](map: java.util.HashMap[K, V]) = {
    val builder = Map.newBuilder[K, V]
    map forEach { (k, v) => builder += k -> v }
    builder.result()
  }

}