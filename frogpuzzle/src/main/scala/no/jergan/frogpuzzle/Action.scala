package no.jergan.frogpuzzle

/**
 * Action of player.
 *
 * @author <a href="mailto:oyvind@jergan.no">Oyvind Jergan</a>
 */
sealed trait Action {

   def character : Char
}

case object UP extends Action {val character = 'u'}
case object RIGHT extends Action {val character = 'r'}
case object DOWN extends Action {val character = 'd'}
case object LEFT extends Action {val character = 'l'}

object Action {
   def parse(character : Char) : Action = {
      character match {
         case 'u' => UP
         case 'r' => RIGHT
         case 'd' => DOWN
         case 'l' => LEFT
      }
   }
}