package no.jergan.frogpuzzle

/**
 * State of frog represented by {@link Position} and {@link Orientation}.
 *
 * @author <a href="mailto:oyvind@jergan.no">Oyvind Jergan</a>
 */
case class State(val position: Position, val orientation: Orientation) {

   override def toString: String = {
      (s"($position - $orientation)");
   }

   def move(action: Option[Action]): State = {
      (action, orientation) match {
         case (None, FACE_UP) | (Some(MOVE_UP), _) => State(position.move(action, orientation), FACE_UP);
         case (None, FACE_RIGHT) | (Some(MOVE_RIGHT), _) => State(position.move(action, orientation), FACE_RIGHT);
         case (None, FACE_DOWN) | (Some(MOVE_DOWN), _) => State(position.move(action, orientation), FACE_DOWN);
         case (None, FACE_LEFT) | (Some(MOVE_LEFT), _) => State(position.move(action, orientation), FACE_LEFT);
      }
   }

}
