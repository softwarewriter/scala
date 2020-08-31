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
         case (None, _) => State(next, orientation)
         case (Some(JUMP), _) => State(next, orientation)
         case (Some(a), _) => State(next, a.orientation.get)
      }
   }

}
