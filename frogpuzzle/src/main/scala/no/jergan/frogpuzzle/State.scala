package no.jergan.frogpuzzle

/**
 * State of frog represented by [[Position]] and [[Orientation]].
 *
 * @author <a href="mailto:oyvind@jergan.no">Oyvind Jergan</a>
 */
case class State(position: Position, orientation: Orientation, fromPortal: Boolean) {

   override def toString: String = {
      s"($position - $orientation)"
   }

   def move(board: Board, action: Option[Action]): State = {
      val portal = if (fromPortal) None else board.portal(position)
      val next = position.move(action, orientation)
      (portal, action, orientation) match {
         case (Some(position), _, _) => State(position, orientation, true)
         case (None, None, _) => State(next, orientation, false)
         case (None, Some(JUMP), _) => State(next, orientation, false)
         case (None, Some(a), _) => State(next, a.orientation.get, false)
      }
   }

}
