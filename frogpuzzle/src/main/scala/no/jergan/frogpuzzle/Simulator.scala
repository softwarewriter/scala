package no.jergan.frogpuzzle

import scala.collection.convert.ImplicitConversionsToJava.`set AsJavaSet`
import scala.collection.mutable

/**
 * Simulates a [[Solution]] to test it.
 *
 * @author <a href="mailto:oyvind@jergan.no">Oyvind Jergan</a>
 */
class Simulator {

   def correctSolution(solution: Solution): Boolean = {
      correctSolution(solution, solution.board.requiredToVisit(), Set.empty, solution.board.initialState());
   }

   @scala.annotation.tailrec
   private[this] def correctSolution(solution: Solution, requiredToVisit: Set[Position], states: Set[State], state: State): Boolean = {
      if (states.contains(state)) {
         return false; // loop
      }
      val position = state.position;
      if (solution.board.squareAt(position) == EMPTY) {
         return false; // outside
      }
      if (position == solution.board.end() && states.map(state => state.position).containsAll(requiredToVisit)) {
         return true;
      }
      correctSolution(solution, requiredToVisit, states + state, state.move(solution.actionAt(position)))
   }

}
