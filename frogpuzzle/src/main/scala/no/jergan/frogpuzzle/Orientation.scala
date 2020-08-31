package no.jergan.frogpuzzle

/**
 * Orientation of frog.
 *
 * @author <a href="mailto:oyvind@jergan.no">Oyvind Jergan</a>
 */
sealed trait Orientation {

   def character: Char
   def deltaX: Int
   def deltaY: Int

   def next(position: Position): Position = {
      Position(position.x + deltaX, position.y + deltaY)
   }

}

case object FACE_UP extends Orientation {
   val character = 'u'
   val deltaX = 0
   val deltaY = -1
}

case object FACE_RIGHT extends Orientation {
   val character = 'r'
   val deltaX = 1
   val deltaY = 0
}

case object FACE_DOWN extends Orientation {
   val character = 'd'
   val deltaX = 0
   val deltaY = 1
}

case object FACE_LEFT extends Orientation {
   val character = 'l'
   val deltaX = -1
   val deltaY = 0
}

object Orientation {
   def parse(character: Char): Orientation = {
      all().find(_.character == character).get
   }

   def all(): List[Orientation] = {
      List(FACE_UP, FACE_RIGHT, FACE_DOWN, FACE_LEFT)
   }

}