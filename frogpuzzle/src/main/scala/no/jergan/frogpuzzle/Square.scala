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

case object PORTAL1 extends Square {
   val character = '1'
}

case object PORTAL2 extends Square {
   val character = '2'
}

case object PORTAL3 extends Square {
   val character = '3'
}
object Square {
   def parse(character: Char): Square = {
      all().find(_.character == character).get
   }

   def all(): List[Square] = {
      List.concat(List(START, END, REGULAR, EMPTY), portals())
   }

   def portals(): List[Square] = {
      List(PORTAL1, PORTAL2, PORTAL3)
   }
}
