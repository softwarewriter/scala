package no.jergan.frogpuzzle

import scala.collection.mutable

/**
 * Board of game where frog lives.
 *
 * @author <a href="mailto:oyvind@jergan.no">Oyvind Jergan</a>
 */
class Board(val squares: List[List[Square]], val start: Position, val end: Position, val orientation: Orientation, val portals: Map[Position, Position]) {

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

   def sizeX(): Int = {
      squares.map(row => row.length).max
   }

   def sizeY(): Int = {
      squares.length
   }

   def requiredToVisit(): Set[Position] = {
      Board.findAll(squares, REGULAR)
   }

   def initialState(): State = {
      State(start, orientation, false)
   }

   def squareAt(position: Position): Square = {
      Board.squareAt(squares, position)
   }

   def portal(position: Position): Option[Position] = {
      portals.get(position)
   }

}

object Board {
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

   private[this] def stringToMatrix(string: String) : List[List[Square]] = {
      val nonEmptyLines = string.split("\n").filter(line => !line.isEmpty)
      val maxSizeX = nonEmptyLines.map(line => line.length).max
      val squares = Array.fill[Square](nonEmptyLines.length + 2, maxSizeX + 2)(EMPTY)
      nonEmptyLines.zipWithIndex foreach { case (line, y) =>
         line.toCharArray.zipWithIndex foreach { case (character, x) =>
            squares(y + 1)(x + 1) = Square.parse(character)
         }
      }
      squares.toList.map(row => row.toList)
   }

   private[this] def findOne(squares: List[List[Square]], square: Square): Either[String, Position] = {
      val all = findAll(squares, square)
      all.size match {
         case 1 => Right(all.iterator.next())
         case _ => Left(s"${all.size} squares of type $square, should only be exactly one")
      }
   }

   def findAll(squares: List[List[Square]], square: Square): Set[Position] = {
      val result = new mutable.HashSet[Position]
      for (y <- squares.indices; x <- squares(y).indices) {
         if (squares(y)(x) == square) {
            result.addOne(Position(x, y))
         }
      }
      result.toSet
   }

   def squareAt(squares: List[List[Square]], position: Position): Square = {
      squares(position.y)(position.x)
   }

   private[this] def initialOrientation(squares: List[List[Square]], start: Position): Either[String, Orientation] = {
      val neighbours = Orientation.all().filter(orientation => squareAt(squares, start.move(None, orientation)) != EMPTY)
      neighbours.size match {
         case 1 => Right(neighbours.iterator.next())
         case _ => Left(s"${neighbours.size} neighbouring squares, should be exactly one")
      }
   }

   private[this] def buildPortalMap(squares: List[List[Square]]) : Either[String, Map[Position, Position]] = {
      Square.portals()
         .map(portalSquare => findAll(squares, portalSquare).toList)
         .map(element => {
            element.size match {
               case 0 => Right(Map.empty)
               case 2 => Right(Map.empty + (element(0) -> element(1)) + (element(1) -> element(0)))
               case other => Left(s"Unmached portals, had $other occurrences")
            }
         })
         .partitionMap(identity) match {
         case (Nil, rights) => Right(rights.flatten.toMap)
         case (lefts, _) => Left(lefts.collectFirst(left => left).get)
      }
   }

}
