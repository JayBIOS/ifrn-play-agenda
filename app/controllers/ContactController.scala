package controllers

import javax.inject._
import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import play.api.Play.current
import play.api.i18n.Messages.Implicits._


case class Contact(name: String, email: String, phone: String)

object Agenda {

  class GetResult(contact: Option[Contact]) {

    val instance: Option[Contact] = contact

    def map: Map[String, String] = {
      if (instance.isEmpty) {
        return Map()
      } else {
        return Map(
          "name" -> instance.get.name,
          "email" -> instance.get.email,
          "phone" -> instance.get.phone
        )
      }
    }

  }

  var contacts: List[Contact] = List()

  def get(id: Int): GetResult = {
    if (contacts.size > id)
      return new GetResult(Option(contacts(id)))
    return new GetResult(None)
  }

  def add(contact: Contact) = {
    contacts = contact :: contacts
  }

  def remove(id: Int) = {
    contacts = if (contacts.size < id) contacts
               else contacts.take(id) ++ contacts.drop(id+1)
  }

  def update(id: Int, contact: Contact) = {
    contacts = contacts.updated(id, contact)
  }

}

@Singleton
class ContactController @Inject() extends Controller {

  val form = Form(
      mapping(
        "name" -> nonEmptyText,
        "email" -> email,
        "phone" -> text
      )(Contact.apply)(Contact.unapply)
  )

  def headers = List(
      "Access-Control-Allow-Origin" -> "*",
      "Access-Control-Allow-Methods" -> "GET, POST, OPTIONS, DELETE, PUT",
      "Access-Control-Max-Age" -> "3600",
      "Access-Control-Allow-Headers" -> "Origin, Content-Type, Accept, Authorization",
      "Access-Control-Allow-Credentials" -> "true"
  )

  def options = Action { request =>
      NoContent.withHeaders(headers : _*)
  }

  def create = Action { implicit request =>
    Ok(views.html.Contact.create(form))
  }

  def createPost = Action { implicit request =>
    form.bindFromRequest.fold(
      formWithErrors => {
        BadRequest(views.html.Contact.create(formWithErrors))
      },
      contact => {
        Agenda.add(contact)
        Redirect(routes.ContactController.index())
      }
    )
  }

  def edit(id: Int) = Action { implicit request =>
    val contact = Agenda.get(id)
    if (contact.instance.isEmpty) {
      Redirect(routes.ContactController.index())
    } else {
      Ok(views.html.Contact.edit(id)(form.bind(contact.map)))
    }
  }

  def editPost(id: Int) = Action { implicit request =>
    form.bindFromRequest.fold(
      formWithErrors => {
        BadRequest(views.html.Contact.edit(id)(formWithErrors))
      },
      contact => {
        Agenda.update(id, contact)
        Redirect(routes.ContactController.index())
      }
    )
  }

  def remove(id: Int) = Action { implicit request => 
    Agenda.remove(id)
    NoContent
  }

  def redirect = Action {
    Redirect(routes.ContactController.index())
  }

  def index = Action { implicit request =>
    Ok(views.html.Contact.index(Agenda.contacts))
  }

}
