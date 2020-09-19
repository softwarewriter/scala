package no.jergan

/**
 * Package.
 *
 * @author <a href="mailto:oyvind@jergan.no">Oyvind Jergan</a>
 */
package object frogpuzzle {

   trait Stringable[A] {

      def asString(a: A): String

   }

   object StringableWrapper {

      def asString[A](a: A)(implicit stringable: Stringable[A]): String  = {
         stringable.asString(a)
      }
   }

   implicit var positionStringable = new Stringable[Position] {
      override def asString(position: Position): String = s"(${position.x}, ${position.y})"
   }

   implicit var orientationStringable = new Stringable[Orientation] {
      override def asString(orientation: Orientation): String = orientation.toString
   }

   implicit var stateStringable = new Stringable[State] {
      override def asString(state: State): String = s"(${positionStringable.asString(state.position)} - ${orientationStringable.asString(state.orientation)})"
   }

}
