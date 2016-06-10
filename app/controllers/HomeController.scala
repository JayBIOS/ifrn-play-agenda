package controllers

import javax.inject._
import play.api._
import play.api.mvc._
import play.api.data._


@Singleton
class HomeController @Inject() extends Controller {

  def redirect = Action {
    Redirect(routes.ContactController.index())
  }
  
}
