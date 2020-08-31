package no.jergan.frogpuzzle

/**
 * Solution (actual or candidate) in context of a [[Board]].
 *
 * @author <a href="mailto:oyvind@jergan.no">Oyvind Jergan</a>
 */
class Solution private(val board: Board, positionToAction: Map[Position, Action]) {

   def addAction(x: Int, y: Int, action: Action): Either[String, Solution] = {
      addAction(Position(x, y), action)
   }

   def addAction(position: Position, action: Action): Either[String, Solution] = {
      val existing = positionToAction.get(position)
      if (existing.isDefined) {
         Left(s"Can't add action to position $position as position already has action $existing")
      }
      val square = board.squareAt(position)
      if (square != REGULAR) {
         Left(s"Actions can only be put on regular squares, was $square")
      }
      Right(new Solution(board, positionToAction + (position -> action)))
   }

   def actionAt(position: Position): Option[Action] = {
      positionToAction.get(position)
   }
}

object Solution {
   def empty(board: Board): Solution = {
      new Solution(board, Map.empty)
   }

   def build(board: Board, values: Tuple3[Int, Int, Action]*) : Either[String, Solution] = {
      val zero: Either[String, Solution] = Right(empty(board))
      values.foldRight(zero) ((element: Tuple3[Int, Int, Action], solution) => {
         solution match {
            case Left(value) => Left(value)
            case Right(value) => value.addAction(element._1, element._2, element._3)
         }
      })
   }

}
