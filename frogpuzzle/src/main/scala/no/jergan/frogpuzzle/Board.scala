package no.jergan.frogpuzzle

/**
 * Board of game.
 *
 * @author <a href="mailto:oyvind@jergan.no">Oyvind Jergan</a>
 */
class Board(val sizeX : Int, val sizeY : Int, board : Array[Array[Square]]) {


}

object Board {
   def parse(string : String) : Board = {
      val nonEmptyLines = string.split("\n").filter(line => !line.isEmpty)
      val maxSizeX = nonEmptyLines.map(line => line.length).max
      val sizeWithBorderX = maxSizeX + 2;
      val sizeWithBorderY = nonEmptyLines.length + 2;

      val board = Array.ofDim[Square](sizeWithBorderY, sizeWithBorderX)
      nonEmptyLines.zipWithIndex foreach { case(line, y) =>
         line.toCharArray.zipWithIndex foreach { case(character, x) =>
//            println(s"($x, $y): " + character);
            board(y)(x) = Square.parse(character)
         }
      }
      new Board(sizeWithBorderX, sizeWithBorderY, board)
   }
}
