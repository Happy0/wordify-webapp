package controllers

import play.api._
import play.api.mvc._
import scrabble.{Board, Pos, Letter}

object Application extends Controller {
  
  def index = Action {
    Ok(views.html.index("Your new application is ready."))
  }
  
  def watcher = Action{
    Ok(views.html.Game.watcher(Board.init.placeLetter(Pos.posAt(1,4).get, Letter('A', 1) ).get ) )
  }
  
}