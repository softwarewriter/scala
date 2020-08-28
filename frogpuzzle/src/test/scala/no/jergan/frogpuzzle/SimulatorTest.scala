package no.jergan.frogpuzzle

import org.scalatest.flatspec.AnyFlatSpec

/**
 * Unit test of {@link Simulator}
 *
 * @author <a href="mailto:oyvind@jergan.no">Oyvind Jergan</a>
 * @version
 */
class SimulatorTest extends AnyFlatSpec {

   behavior of "correctSolution"

   it should "be able to identify correct solution" in {
      val board = FrogPuzzleTest.testBoard()
      val solution = Solution.empty(board)
         .addAction(1, 2, MOVE_RIGHT)
         .addAction(2, 2, MOVE_DOWN)
         .addAction(2, 3, MOVE_RIGHT)
         .addAction(3, 3, MOVE_UP)
      assert(new Simulator().correctSolution(solution))
   }

   it should "be able to identify non correct solution" in {
      val board = FrogPuzzleTest.testBoard()
      val solution = Solution.empty(board)
         .addAction(1, 2, MOVE_RIGHT)
         .addAction(2, 2, MOVE_DOWN)
         .addAction(2, 3, MOVE_RIGHT)
      assert(!new Simulator().correctSolution(solution))
   }

}
