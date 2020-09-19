package no.jergan.frogpuzzle


/**
 * What does this class do?
 *
 * @author <a href="mailto:oyvind@jergan.no">Oyvind Jergan</a>
 */
object Main {

   def main(args: Array[String]): Unit = {
      val position = Position(1, 4)
      val state = State(position, FACE_UP, false)

      import no.jergan.frogpuzzle.StringableWrapper.asString

      println(positionStringable.asString(position))
      println(asString(position))
      println(asString(state))
   }

}

