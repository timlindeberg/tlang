package controllers

import javax.inject.{Inject, Singleton}
import play.api.mvc._

@Singleton
class IndexController @Inject()(cc: ControllerComponents, assetController: Assets) extends AbstractController(cc) {

  private val Index = "index.html"

  def index(path: String): Action[AnyContent] = assetController.at(Index)
}
