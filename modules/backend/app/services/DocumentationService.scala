package services

import java.nio.file.{Files, Path, Paths}
import javax.inject._

import javax.inject.Inject
import play.api.Configuration

import scala.io.Source
import scala.collection.JavaConverters._

case class Documentation(name: String, content: String)

@Singleton
class DocumentationService  @Inject() (config: Configuration) {

  def documentation: List[Documentation] = {
    def documentationPath = Paths.get(config.get[String]("tlang.documentation.path"))
    Files.walk(documentationPath)
      .iterator
      .asScala
      .filter(_.toString.endsWith(".md"))
      .toList
      .sortBy(_.toString)
      .map { path =>
        val source = Source.fromFile(path.toFile)
        val content = try source.mkString finally source.close()
        Documentation(markdownFileName(path), content)
      }
  }

  private def markdownFileName(path: Path): String = {
    val fileName = path.getFileName.toString
    fileName.replaceAll("""^(\d+)_""", "").replaceAll("""\.md$""", "")
  }


}
