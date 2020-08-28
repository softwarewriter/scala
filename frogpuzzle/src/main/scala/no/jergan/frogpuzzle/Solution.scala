package no.jergan.frogpuzzle

/**
 * Solution (actual or candidate) in context of a {@link Board}.
 *
 * @author <a href="mailto:oyvind@jergan.no">Oyvind Jergan</a>
 */
class Solution private (val board : Board, positionToAction: Map[Position, Action]) {

   def addAction(x : Int, y: Int , action : Action): Solution = {
      addAction(Position(x, y), action);
   }

   def addAction(position: Position, action: Action): Solution = {
      val existing = positionToAction.get(position);
      if (existing.isDefined)
      {
         throw new Exception("Can't add action to position " + position + " as position already has action " + existing);
      }
      val square = board.squareAt(position);
      if (square != REGULAR)
      {
         throw new Exception("Actions can only be put on regular squares, was " + square);
      }
      new Solution(board, positionToAction + (position -> action))
   }

   def actionAt(position : Position) : Option[Action] = {
      positionToAction.get(position)
   }
}

object Solution {
   def empty(board: Board): Solution = {
      new Solution(board, Map.empty)
   }
}
