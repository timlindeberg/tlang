package controllers

import javax.inject._
import play.api.mvc._
import play.api.libs.json.{Json, OWrites}
import services.{Documentation, DocumentationService}

@Singleton
class DocumentationController @Inject()(cc: ControllerComponents, documentationService: DocumentationService) extends AbstractController(cc) {

  private implicit val documentationWrites: OWrites[Documentation] = Json.writes[Documentation]

  def documentation: Action[AnyContent] = Action {
    def documentation = documentationService.documentation

    Ok(
      Json.obj {
        "markdown" -> documentation.map { doc => Json.toJson(doc) }
      }
    )
  }
}
