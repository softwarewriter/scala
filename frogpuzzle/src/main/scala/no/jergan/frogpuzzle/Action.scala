package no.jergan.frogpuzzle

/**
 * Action of frog.
 *
 * @author <a href="mailto:oyvind@jergan.no">Oyvind Jergan</a>
 */
sealed trait Action {

   def character : Char
}

case object MOVE_UP extends Action {val character = 'u'}
case object MOVE_RIGHT extends Action {val character = 'r'}
case object MOVE_DOWN extends Action {val character = 'd'}
case object MOVE_LEFT extends Action {val character = 'l'}

object Action {
   def parse(character : Char) : Action = {
      character match {
         case 'u' => MOVE_UP
         case 'r' => MOVE_RIGHT
         case 'd' => MOVE_DOWN
         case 'l' => MOVE_LEFT
      }
   }
}