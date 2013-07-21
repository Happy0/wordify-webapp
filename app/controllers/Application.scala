package controllers

import play.api._
import play.api.mvc._
import scrabble.Board

object Application extends Controller {
  
  def index = Action {
    Ok(views.html.index("Your new application is ready."))
  }
  
  def watcher = Action{
    Ok(views.html.Game.watcher(Board.init))
  }
  
}