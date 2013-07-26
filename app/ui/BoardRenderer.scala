package ui

import scrabble.{
  Board,
  Pos,
  Square,
  NormalSquare,
  DoubleLetterSquare,
  TripleLetterSquare,
  DoubleWordSquare,
  TripleWordSquare,
  Tile
}

object BoardRenderer {

  case class RenderPosition(pos: Pos, top: Int, left: Int)

  val renderPositions = {
    def toRenderPosition(pos: Pos) = RenderPosition(pos, (pos.y - 1) * 32, (pos.x - 1) * 32)

    val positionsOrdered = Pos.allPositions.map(x => x._2).toList.sortBy { case Pos(x, y, _) => (x, y) }

    positionsOrdered map toRenderPosition
  }

  /* @TODO: In the non-prototype version this should take a game in progress so that it can use some of
   *  the state to show things like the last move */
  def render(board: Board): String = {

    def renderTile(tile: Tile) = {
      val tileValue = """<div class = "value"><sub>%s</sub></div>""".format(tile.value)

      """<div class="tile %s" id="%s"><div class="lettertext">%s</div>%s</div> """.format("letter", tile.letter,
        tile.letter, tileValue)
    }

    val squares = for {
      render <- renderPositions.iterator
      sq <- board squareAt render.pos
      tile = sq.tile

      (classText, squareText) = sq match {
        case NormalSquare(_) => ("normal", "")
        case DoubleLetterSquare(_) => ("doubleletter", "DL")
        case TripleLetterSquare(_) => ("tripleletter", "TL")
        case DoubleWordSquare(_) => ("doubleword", "DW")
        case TripleWordSquare(_) => ("tripleword", "TW")
        case _ => ("", "")
      }

      squareLabel = """<div class="specialtext">%s</div>""".format(squareText)

      squareContains = sq.tile.fold(squareLabel)(renderTile)

      squareDiv = """<div class="square %s" id="%s" style="top:%dpx;left:%dpx;"> """.format(
        classText,
        render.pos.gridCordinates,
        render.top,
        render.left) ++ squareContains ++ "</div>"

    } yield squareDiv

    squares mkString ("\n")
  }
}