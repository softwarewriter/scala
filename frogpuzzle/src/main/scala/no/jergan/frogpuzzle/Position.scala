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
         case (None, _) => orientation.next(this)
         case (Some(JUMP), _) => orientation.next(orientation.next(this))
         case (Some(a), _) => a.orientation.get.next(this)
      }
   }

}
