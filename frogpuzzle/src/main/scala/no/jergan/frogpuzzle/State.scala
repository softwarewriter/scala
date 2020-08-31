package no.jergan.frogpuzzle

/**
 * State of frog represented by [[Position]] and [[Orientation]].
 *
 * @author <a href="mailto:oyvind@jergan.no">Oyvind Jergan</a>
 */
case class State(position: Position, orientation: Orientation) {

   override def toString: String = {
      s"($position - $orientation)"
   }

   def move(action: Option[Action]): State = {
      val next = position.move(action, orientation)
      (action, orientation) match {
         case (None, FACE_UP) | (Some(MOVE_UP), _) => State(next, FACE_UP)
         case (None, FACE_RIGHT) | (Some(MOVE_RIGHT), _) => State(next, FACE_RIGHT)
         case (None, FACE_DOWN) | (Some(MOVE_DOWN), _) => State(next, FACE_DOWN)
         case (None, FACE_LEFT) | (Some(MOVE_LEFT), _) => State(next, FACE_LEFT)
         case (Some(JUMP), _) => State(next, orientation)
      }
   }

}
