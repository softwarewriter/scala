package no.jergan.frogpuzzle

import no.jergan.frogpuzzle.Board.Matrix

import scala.collection.mutable

/**
 * Board of game where frog lives.
 *
 * @author <a href="mailto:oyvind@jergan.no">Oyvind Jergan</a>
 */
class Board(val matrix: Matrix, val start: Position, val end: Position, val orientation: Orientation, val portals: Map[Position, Position]) {

   override def toString: String = {
      val result = new StringBuilder()
      matrix.foreach(row => {
         row.foreach(square => {
            result.addOne(square.character)
         })
         result.addOne('\n')
      })
      result.toString
   }

   def sizeX(): Int = {
      matrix.map(row => row.length).max
   }

   def sizeY(): Int = {
      matrix.length
   }

   def requiredToVisit(): Set[Position] = {
      Board.findAll(matrix, REGULAR)
   }

   def initialState(): State = {
      State(start, orientation, false)
   }

   def squareAt(position: Position): Square = {
      Board.squareAt(matrix, sizeX(), sizeY(), position)
   }

   def portal(position: Position): Option[Position] = {
      portals.get(position)
   }

}

object Board {

   type Matrix = List[List[Square]]

   def parse(string: String): Either[String, Board] = {
      val squares = stringToMatrix(string)
      val start = findOne(squares, START)
      val end = findOne(squares, END)
      val orientation: Either[String, Orientation] = start match {
         case Left(value) => Left("No start")
         case Right(value) => initialOrientation(squares, value)
      }
      val portals = buildPortalMap(squares);
      (start, end, orientation, portals) match {
         case (Right(start), Right(end), Right(orientation), Right(portals)) => Right(new Board(squares, start, end, orientation, portals))
         case (_, _, _, _) => Left("Board does not meet initial requirements")
      }
   }

   private[this] def stringToMatrix(string: String) : Matrix = {
      val nonEmptyLines = string.split("\n").filter(line => !line.isEmpty)
      val maxSizeX = nonEmptyLines.map(line => line.length).max
      val squares = Array.fill[Square](nonEmptyLines.length, maxSizeX)(EMPTY)
      nonEmptyLines.zipWithIndex foreach { case (line, y) =>
         line.toCharArray.zipWithIndex foreach { case (character, x) =>
            squares(y)(x) = Square.parse(character)
         }
      }
      squares.toList.map(row => row.toList)
   }

   private[this] def findOne(matrix: Matrix, square: Square): Either[String, Position] = {
      val all = findAll(matrix, square)
      all.size match {
         case 1 => Right(all.iterator.next())
         case _ => Left(s"${all.size} squares of type $square, should only be exactly one")
      }
   }

   def findAll(matrix: Matrix, square: Square): Set[Position] = {
      val result = new mutable.HashSet[Position]
      for (y <- matrix.indices; x <- matrix(y).indices) {
         if (matrix(y)(x) == square) {
            result.addOne(Position(x, y))
         }
      }
      result.toSet
   }

   def squareAt(matrix: Matrix, sizeX: Int, sizeY: Int, position: Position): Square = {
      if (position.x < 0 || position.x >= sizeX || position.y < 0 || position.y >= sizeY) EMPTY else matrix(position.y)(position.x)
   }

   private[this] def initialOrientation(matrix: Matrix, start: Position): Either[String, Orientation] = {
      val neighbours = Orientation.all().filter(orientation => squareAt(matrix, matrix.map(row => row.length).max, matrix.length, start.move(None, orientation)) != EMPTY)
      neighbours.size match {
         case 1 => Right(neighbours.iterator.next())
         case _ => Left(s"${neighbours.size} neighbouring squares, should be exactly one")
      }
   }

   private[this] def buildPortalMap(matrix: Matrix) : Either[String, Map[Position, Position]] = {
      Square.portals()
         .map(portalSquare => findAll(matrix, portalSquare).toList)
         .map(element => {
            element.size match {
               case 0 => Right(Map.empty)
               case 2 => Right(Map.empty + (element(0) -> element(1)) + (element(1) -> element(0)))
               case other => Left(s"Unmached portals, had $other occurrences, should be exactly two")
            }
         })
         .partitionMap(identity) match {
         case (Nil, rights) => Right(rights.flatten.toMap)
         case (lefts, _) => Left(lefts.collectFirst(left => left).get)
      }
   }

}
