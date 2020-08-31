package no.jergan.frogpuzzle

/**
 * Position on [[Board]].
 *
 * @author <a href="mailto:oyvind@jergan.no">Oyvind Jergan</a>
 */
case class Position(x: Int, y: Int) {

   override def toString: String = {
      s"($x, $y)"
   }

   def move(action: Option[Action], orientation: Orientation): Position = {
      (action, orientation) match {
         case (None, FACE_UP) | (Some(MOVE_UP), _) => FACE_UP.next(this)
         case (None, FACE_RIGHT) | (Some(MOVE_RIGHT), _) => FACE_RIGHT.next(this)
         case (None, FACE_DOWN) | (Some(MOVE_DOWN), _) => FACE_DOWN.next(this)
         case (None, FACE_LEFT) | (Some(MOVE_LEFT), _) => FACE_LEFT.next(this)
         case (Some(JUMP), _) => orientation.next(orientation.next(this))
      }
   }

}
