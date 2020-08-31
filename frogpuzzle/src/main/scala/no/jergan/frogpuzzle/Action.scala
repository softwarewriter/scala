package no.jergan.frogpuzzle

/**
 * Action of frog.
 *
 * @author <a href="mailto:oyvind@jergan.no">Oyvind Jergan</a>
 */
sealed trait Action {
   def character: Char
   def orientation: Option[Orientation]
}

case object MOVE_UP extends Action {
   val character = 'u'
   val orientation = Some(FACE_UP)
}

case object MOVE_RIGHT extends Action {
   val character = 'r'
   val orientation = Some(FACE_RIGHT)
}

case object MOVE_DOWN extends Action {
   val character = 'd'
   val orientation = Some(FACE_DOWN)
}

case object MOVE_LEFT extends Action {
   val character = 'l'
   val orientation = Some(FACE_LEFT)
}

case object JUMP extends Action {
   val character = 'j'
   val orientation = None
}

object Action {
   def parse(character: Char): Action = {
      all().find(_.character == character).get
   }

   def all(): List[Action] = {
      List(MOVE_UP, MOVE_RIGHT, MOVE_DOWN, MOVE_LEFT)
   }

}