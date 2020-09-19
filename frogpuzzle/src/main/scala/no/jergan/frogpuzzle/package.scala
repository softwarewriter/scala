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
   // @kaare:
   //  Dette er signaturen vi brukte på fredag:
   //    implicit class StringableSyntax[A: Stringable](val a: A) {

   implicit var positionStringable = new Stringable[Position] {
      override def asString(position: Position): String = s"(${position.x}, ${position.y})"
   }

   // Variant i kortform (single abstract method)
   implicit var orientationStringable: Stringable[Orientation] = (orientation: Orientation) => orientation.toString

   implicit var stateStringable: Stringable[State] = new Stringable[State] {
      override def asString(state: State): String = s"(${positionStringable.asString(state.position)} - ${orientationStringable.asString(state.orientation)})"
   }

}
