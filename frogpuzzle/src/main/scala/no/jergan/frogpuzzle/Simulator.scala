package no.jergan.frogpuzzle

import scala.collection.mutable

/**
 * Simulates a [[Solution]] to test it.
 *
 * @author <a href="mailto:oyvind@jergan.no">Oyvind Jergan</a>
 */
class Simulator {

   def correctSolution(solution: Solution): Boolean = {
      val unvisited = new mutable.HashSet[Position].addAll(solution.board.requiredToVisit())
      val states = new mutable.HashSet[State]
      correctSolution(solution, states, unvisited, solution.board.initialState());
   }

   private[this] def correctSolution(solution: Solution, states: mutable.HashSet[State], unvisited: mutable.HashSet[Position], state: State): Boolean = {
      if (states.contains(state)) {
         return false; // loop
      }
      val position = state.position;
      if (position == solution.board.end() && unvisited.isEmpty) {
         return true;
      }
      if (solution.board.squareAt(position) == EMPTY) {
         return false;
      }
      val removed = unvisited.remove(position);
      val result = correctSolution(solution, states + state, unvisited, state.move(solution.actionAt(position)))
      if (removed) {
         unvisited.add(position);
      }
      result;
   }

}