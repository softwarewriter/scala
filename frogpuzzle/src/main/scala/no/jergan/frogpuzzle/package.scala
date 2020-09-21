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

   // @kaare: Det er her jeg lurer på hvorfor vi hadde "implicit class StringableSyntax[A: Stringable](val a: A)"
   //         når signaturen under ser ut til å gjøre det jeg vil.
   implicit class StringableSyntax[A](val a: A) {
      def asStringInSyntax(implicit stringable: Stringable[A]): String = stringable.asString(a)
   }

   implicit var positionStringable = new Stringable[Position] {
      override def asString(position: Position): String = s"(${position.x}, ${position.y})"
   }

   // Variant i kortform (single abstract method)
   implicit var orientationStringable: Stringable[Orientation] = (orientation: Orientation) => orientation.toString

   // @kaare: Det er de to konstruksjonene under jeg ønsker å slå sammen hvis mulig.

   class StateStringable(implicit positionStringable: Stringable[Position], implicit val orientationStringable: Stringable[Orientation]) extends Stringable[State] {
      override def asString(state: State): String = s"(${positionStringable.asString(state.position)} - ${orientationStringable.asString(state.orientation)})"
   }

   implicit var stateStringable = new StateStringable()

}
