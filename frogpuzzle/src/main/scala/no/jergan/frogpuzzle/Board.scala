package no.jergan.frogpuzzle

import scala.collection.mutable.ListBuffer

/**
 * Board of game.
 *
 * @author <a href="mailto:oyvind@jergan.no">Oyvind Jergan</a>
 */
class Board(val sizeX : Int, val sizeY : Int, board : Array[Array[Square]]) {

   override def toString : String = {
      val result = new StringBuilder();
      board.foreach(row => {
         row.foreach(square => {
            result.addOne(square.character)
         })
         result.addOne('\n')
      })
      result.toString
  }

   def start() : Position = {
      findOne(START)
   }

   def end() : Position = {
      findOne(END)
   }

   def findOne(square: Square): Position = {
      val all = findAll(square)
      if (all.isEmpty) {
         throw new Exception("No square of type " + square);
      }
      if (all.size > 1)
      {
         throw new Exception("" + all.size + " squares of type " + square);
      }
      all.iterator.next();
   }

   def findAll(square : Square) : List[Position] = {
      val result = new ListBuffer[Position]
      for (y <- 0 to sizeY - 1; x <- 0 to sizeX - 1) {
         val s = board(y)(x)
         if (s == square) {
            result.addOne(new Position(x, y))
         }
      }
      result.toList
   }

}

object Board {
   def parse(string : String) : Board = {
      val nonEmptyLines = string.split("\n").filter(line => !line.isEmpty)
      val maxSizeX = nonEmptyLines.map(line => line.length).max
      val sizeWithBorderX = maxSizeX + 2;
      val sizeWithBorderY = nonEmptyLines.length + 2;
      val board = Array.fill[Square](sizeWithBorderY, sizeWithBorderX)(EMPTY)
      nonEmptyLines.zipWithIndex foreach { case(line, y) =>
         line.toCharArray.zipWithIndex foreach { case(character, x) =>
            board(y + 1)(x + 1) = Square.parse(character)
         }
      }
      new Board(sizeWithBorderX, sizeWithBorderY, board)
   }
}
