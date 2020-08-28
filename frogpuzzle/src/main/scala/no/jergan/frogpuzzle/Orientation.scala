package no.jergan.frogpuzzle

/**
 * Orientation of frog.
 *
 * @author <a href="mailto:oyvind@jergan.no">Oyvind Jergan</a>
 */
sealed trait Orientation {

   def character : Char
}

case object FACE_UP extends Orientation {val character = 'u'}
case object FACE_RIGHT extends Orientation {val character = 'r'}
case object FACE_DOWN extends Orientation {val character = 'd'}
case object FACE_LEFT extends Orientation {val character = 'l'}

object Orientation {
   def parse(character: Char): Orientation = {
      character match {
         case FACE_UP.character => FACE_UP
         case FACE_RIGHT.character => FACE_RIGHT
         case FACE_DOWN.character => FACE_DOWN
         case FACE_LEFT.character => FACE_LEFT
      }
   }

   def all(): List[Orientation] = {
      List(FACE_UP, FACE_RIGHT, FACE_DOWN, FACE_LEFT)
   }

}