package no.jergan.frogpuzzle

/**
 * Position on board.
 *
 * @author <a href="mailto:oyvind@jergan.no">Oyvind Jergan</a>
 */
class Position(val x : Int, val y : Int) {

   override def toString : String = {
      "($x, $y)";
   }

   def move(action: Option[Action], orientation: Orientation) : Position = {
      (action, orientation) match {
         case (None, FACE_UP) | (Some(MOVE_UP), _) => new Position(x, y - 1);
         case (None, FACE_RIGHT) | (Some(MOVE_RIGHT), _) => new Position(x + 1, y);
         case (None, FACE_DOWN) | (Some(MOVE_DOWN), _) => new Position(x, y + 1);
         case (None, FACE_LEFT) | (Some(MOVE_LEFT), _) => new Position(x - 1, y);
      }
   }

}
