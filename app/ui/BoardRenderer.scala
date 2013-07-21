package ui

import scrabble.Board
import scrabble.Pos
import scrabble.Square
import scrabble.NormalSquare
import scrabble.DoubleLetterSquare
import scrabble.TripleLetterSquare
import scrabble.DoubleWordSquare
import scrabble.TripleWordSquare

object BoardRenderer {

  def toRenderPosition(pos: Pos) = RenderPosition(pos, pos.x * 32, pos.y * 32)

  case class RenderPosition(pos: Pos, top: Int, left: Int)

  val positionsOrdered = Pos.allPositions.map(x => x._2).toList.sortBy { case Pos(x, y, _) => (x, y) }

  val renderPositions = positionsOrdered map toRenderPosition

  /* @TODO: In the non-prototype version this should take a game in progress so that it can use some of
   *  the state to show things like the last move */
  def render(board: Board) : String = {

    val squares = for {
      render <- renderPositions
      sq <- board squareAt render.pos
      tile = sq.tile

      str = sq.tile.fold {
        val (classText, text) = sq match {
          case NormalSquare(_) => ("normal", "")
          case DoubleLetterSquare(_) => ("doubleletter", "DL")
          case TripleLetterSquare(_) => ("tripleletter", "TL")
          case DoubleWordSquare(_) => ("doubleword", "DW")
          case TripleWordSquare(_) => ("tripleword", "TW")
          case _ => ("", "")
        }

        """<div class="square %s" id="%s" style="top:%dpx;left:%dpx;"> """.format(
          classText,
          render.pos.gridCordinates,
          render.top,
          render.left) ++ text ++ " </div>"

      } { tl =>
        ""
      }

    } yield { str }

    squares mkString
  }
}