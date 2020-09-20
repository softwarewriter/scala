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
      val c2: Int => Int = curried(1)(2, 3)
      val c3: Int = curried(1)(2, 3)(4)


      def intToString1(a: Int): String = a.toString
      def intToString2: Int => String = (a: Int) => a.toString

      def stringToInt1(a: String): Int = a.length
      def stringToInt2: String => Int = (a: String) => a.length

      def manualComposition(a: Int): Int = {
         stringToInt1(intToString1(a))
      }

      def comp2: Int => Int = manualComposition

      def comp3 = stringToInt2 compose intToString2
      def comp4 = intToString2 andThen stringToInt2

      // Disse kompilerer ikke.
      //def comp5 = stringToInt1 compose intToString1
      //def comp6 = intToString1 andThen stringToInt1

      // Trolig pga av disse heller ikke kompilerer, men hvorfor
      // def intToString3 = intToString1
      // def stringToInt3 = stringToInt1

      // Men det gjør disse
      def intToString3 = intToString1 _
      def stringToInt3 = stringToInt1 _

      // Og disse
      def intToString4: Int => String = intToString1
      def stringToInt4: String => Int = stringToInt1

   }

}

