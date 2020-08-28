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
         case (None, FACE_UP) | (Some(MOVE_UP), _) => Position(x, y - 1)
         case (None, FACE_RIGHT) | (Some(MOVE_RIGHT), _) => Position(x + 1, y)
         case (None, FACE_DOWN) | (Some(MOVE_DOWN), _) => Position(x, y + 1)
         case (None, FACE_LEFT) | (Some(MOVE_LEFT), _) => Position(x - 1, y)
      }
   }

}
