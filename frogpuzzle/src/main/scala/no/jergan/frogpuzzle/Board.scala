package no.jergan.frogpuzzle

import scala.collection.mutable

/**
 * Board of game where frog lives.
 *
 * @author <a href="mailto:oyvind@jergan.no">Oyvind Jergan</a>
 */
class Board(val sizeX: Int, val sizeY: Int, val squares: Array[Array[Square]], val start: Position, val end: Position) {

   override def toString: String = {
      val result = new StringBuilder()
      squares.foreach(row => {
         row.foreach(square => {
            result.addOne(square.character)
         })
         result.addOne('\n')
      })
      result.toString
   }

   def requiredToVisit(): Set[Position] = {
      findAll(REGULAR)
   }

   def initialState(): State = {
      State(start, initialOrientation())
   }

   def squareAt(position: Position): Square = {
      if (position.x > this.sizeX) {
         throw new Exception(s"${position.x} is outside width of board + $sizeX")
      }
      if (position.y > this.sizeY) {
         throw new Exception(s"${position.y} is outside height of board + $sizeY")
      }
      squares(position.y)(position.x)
   }

   private[this] def findAll(square: Square): Set[Position] = {
      val result = new mutable.HashSet[Position]
      for (y <- 0 until sizeY; x <- 0 until sizeX) {
         if (squares(y)(x) == square) {
            result.addOne(Position(x, y))
         }
      }
      result.toSet
   }

   private[this] def initialOrientation(): Orientation = {
      val neighbours = Orientation.all().filter(orientation => squareAt(start.move(None, orientation)) != EMPTY)
      if (neighbours.isEmpty) {
         throw new Exception("No neighbouring square")
      }
      if (neighbours.size > 2) {
         throw new Exception(s"${neighbours.size} neighbouring squares")
      }
      neighbours.last
   }

}

object Board {
   def parse(string: String): Either[String, Board] = {
      val nonEmptyLines = string.split("\n").filter(line => !line.isEmpty)
      val maxSizeX = nonEmptyLines.map(line => line.length).max
      val sizeWithBorderX = maxSizeX + 2
      val sizeWithBorderY = nonEmptyLines.length + 2
      val squares = Array.fill[Square](sizeWithBorderY, sizeWithBorderX)(EMPTY)
      nonEmptyLines.zipWithIndex foreach { case (line, y) =>
         line.toCharArray.zipWithIndex foreach { case (character, x) =>
            squares(y + 1)(x + 1) = Square.parse(character)
         }
      }
      val start = findOne(squares, START)
      val end = findOne(squares, END)

      (start, end) match {
         case (Right(start), Right(end)) => Right(new Board(sizeWithBorderX, sizeWithBorderY, squares, start, end))
         case (_, _) => Left("Board does not meet initial requirements")
      }
   }

   private[this] def findOne(squares: Array[Array[Square]], square: Square): Either[String, Position] = {
      val all = findAll(squares, square)
      all.size match {
         case 1 => Right(all.iterator.next())
         case _=> Left(s"${all.size} squares of type $square, should only be one")
      }
   }

   private[this] def findAll(squares: Array[Array[Square]], square: Square): Set[Position] = {
      val result = new mutable.HashSet[Position]
      for (y <- squares.indices; x <- squares(y).indices) {
         if (squares(y)(x) == square) {
            result.addOne(Position(x, y))
         }
      }
      result.toSet
   }

}
