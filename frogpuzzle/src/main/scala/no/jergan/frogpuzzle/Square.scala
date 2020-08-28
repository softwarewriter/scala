package no.jergan.frogpuzzle

/**
 * Square of the [[Board]].
 *
 * @author <a href="mailto:oyvind@jergan.no">Oyvind Jergan</a>
 */
sealed trait Square {

   def character: Char
}

case object START extends Square {
   val character = 's'
}

case object END extends Square {
   val character = 'e'
}

case object REGULAR extends Square {
   val character = 'x'
}

case object EMPTY extends Square {
   val character = ' '
}

object Square {
   def parse(character: Char): Square = {
      character match {
         case START.character => START
         case END.character => END
         case REGULAR.character => REGULAR
         case EMPTY.character => EMPTY
      }
   }
}
