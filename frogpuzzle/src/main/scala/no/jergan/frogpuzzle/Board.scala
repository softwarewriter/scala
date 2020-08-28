package no.jergan.frogpuzzle

import scala.collection.mutable

/**
 * Board of game where frog lives.
 *
 * @author <a href="mailto:oyvind@jergan.no">Oyvind Jergan</a>
 */
class Board(val sizeX: Int, val sizeY: Int, board: Array[Array[Square]]) {

   override def toString: String = {
      val result = new StringBuilder();
      board.foreach(row => {
         row.foreach(square => {
            result.addOne(square.character)
         })
         result.addOne('\n')
      })
      result.toString
   }

   def start(): Position = {
      findOne(START)
   }

   def end(): Position = {
      findOne(END)
   }

   def requiredToVisit(): Set[Position] = {
      findAll(REGULAR)
   }

   def initialState(): State = {
      State(start(), initialOrientation());
   }

   def squareAt(position: Position): Square = {
      if (position.x > this.sizeX) {
         throw new Exception(s"${position.x} is outside width of board + $sizeX")
      }
      if (position.y > this.sizeY) {
         throw new Exception(s"${position.y} is outside height of board + $sizeY")
      }
      board(position.y)(position.x);
   }

   private[this] def findOne(square: Square): Position = {
      val all = findAll(square)
      if (all.isEmpty) {
         throw new Exception(s"No square of type $square");
      }
      if (all.size > 1) {
         throw new Exception(s"${all.size} squares of type $square");
      }
      all.iterator.next();
   }

   private[this] def findAll(square: Square): Set[Position] = {
      val result = new mutable.HashSet[Position]
      for (y <- 0 until sizeY; x <- 0 until sizeX) {
         if (board(y)(x) == square) {
            result.addOne(Position(x, y))
         }
      }
      result.toSet
   }

   private[this] def initialOrientation(): Orientation = {
      val neighbours = Orientation.all().filter(orientation => squareAt(start().move(None, orientation)) != EMPTY)
      if (neighbours.isEmpty) {
         throw new Exception("No neighbouring square");
      }
      if (neighbours.size > 2) {
         throw new Exception(s"${neighbours.size} neighbouring squares");
      }
      neighbours.last
   }

}

object Board {
   def parse(string: String): Board = {
      val nonEmptyLines = string.split("\n").filter(line => !line.isEmpty)
      val maxSizeX = nonEmptyLines.map(line => line.length).max
      val sizeWithBorderX = maxSizeX + 2;
      val sizeWithBorderY = nonEmptyLines.length + 2;
      val board = Array.fill[Square](sizeWithBorderY, sizeWithBorderX)(EMPTY)
      nonEmptyLines.zipWithIndex foreach { case (line, y) =>
         line.toCharArray.zipWithIndex foreach { case (character, x) =>
            board(y + 1)(x + 1) = Square.parse(character)
         }
      }
      new Board(sizeWithBorderX, sizeWithBorderY, board)
   }
}
