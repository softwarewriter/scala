package no.jergan

/**
 * Package object.
 *
 * @author <a href="mailto:oyvind@jergan.no">Oyvind Jergan</a>
 */
package object frogpuzzle {

   trait Stringable[A] {

      def asString(a: A): String

   }

   object StringableWrapper {

      def asStringInWrapper[A](a: A)(implicit stringable: Stringable[A]): String  = {
         stringable.asString(a)
      }
   }

   implicit class StringableSyntax[A](val a: A) {
      def asStringInSyntax(implicit stringable: Stringable[A]): String = stringable.asString(a)
   }

   implicit var positionStringable = new Stringable[Position] {
      override def asString(position: Position): String = s"(${position.x}, ${position.y})"
   }

   implicit var orientationStringable = new Stringable[Orientation] {
      override def asString(orientation: Orientation): String = orientation.toString
   }

   // Variant i kortform (single abstract method)
   implicit var stateStringable: Stringable[State] = (state: State) => s"(${positionStringable.asString(state.position)} - ${orientationStringable.asString(state.orientation)})"

}
