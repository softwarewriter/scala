package no.jergan.frogpuzzle


/**
 * Main class for testing.
 *
 * @author <a href="mailto:oyvind@jergan.no">Oyvind Jergan</a>
 */
object Main {

   def main(args: Array[String]): Unit = {
      val position = Position(1, 4)
      val state = State(position, FACE_UP, false)

      import no.jergan.frogpuzzle.StringableWrapper.asStringInWrapper
      

      println(stateStringable.asString(state)) // explicit
      println(asStringInWrapper(state)) // implicit using wrapper
      println(state.asStringInSyntax) // magic using implicit class

      def curried(a: Int)(b: Int, c: Int)(d: Int): Int = {
         a + b + d + d
      }

      val c0: Int => (Int, Int) => Int => Int = curried
      val c1: (Int, Int) => Int => Int = curried(1)
      val c2: (Int => Int) = curried(1)(2, 3)
      val c3: Int = curried(1)(2, 3)(4)




   }

}

