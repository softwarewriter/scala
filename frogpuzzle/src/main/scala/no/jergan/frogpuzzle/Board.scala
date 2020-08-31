package no.jergan.frogpuzzle

import scala.collection.mutable

/**
 * Board of game where frog lives.
 *
 * @author <a href="mailto:oyvind@jergan.no">Oyvind Jergan</a>
 */
class Board(val sizeX: Int, val sizeY: Int, val squares: Array[Array[Square]], val start: Position, val end: Position, val orientation: Orientation) {

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
      Board.findAll(squares, REGULAR)
   }

   def initialState(): State = {
      State(start, orientation)
   }

   def squareAt(position: Position): Square = {
      Board.squareAt(squares, position)
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
      val orientation: Either[String, Orientation] = start match {
         case Left(value) => Left("No start")
         case Right(value) => initialOrientation(squares, value)
      }

      (start, end, orientation) match {
         case (Right(start), Right(end), Right(orientation)) => Right(new Board(sizeWithBorderX, sizeWithBorderY, squares, start, end, orientation))
         case (_, _, _) => Left("Board does not meet initial requirements")
      }
   }

   private[this] def findOne(squares: Array[Array[Square]], square: Square): Either[String, Position] = {
      val all = findAll(squares, square)
      all.size match {
         case 1 => Right(all.iterator.next())
         case _=> Left(s"${all.size} squares of type $square, should only be exactly one")
      }
   }

   def findAll(squares: Array[Array[Square]], square: Square): Set[Position] = {
      val result = new mutable.HashSet[Position]
      for (y <- squares.indices; x <- squares(y).indices) {
         if (squares(y)(x) == square) {
            result.addOne(Position(x, y))
         }
      }
      result.toSet
   }

   private[this] def initialOrientation(squares: Array[Array[Square]], start: Position): Either[String, Orientation] = {
      val neighbours = Orientation.all().filter(orientation => squareAt(squares, start.move(None, orientation)) != EMPTY)
      neighbours.size match {
         case 1 => Right(neighbours.iterator.next())
         case _=> Left(s"${neighbours.size} neighbouring squares, should be exactly one")
      }
   }

   def squareAt(squares: Array[Array[Square]], position: Position): Square = {
      squares(position.y)(position.x)
   }

}
