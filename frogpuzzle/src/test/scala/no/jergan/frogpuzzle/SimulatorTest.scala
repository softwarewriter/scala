package no.jergan.frogpuzzle

import org.scalatest.flatspec.AnyFlatSpec

/**
 * Unit test of [[Simulator]].
 *
 * @author <a href="mailto:oyvind@jergan.no">Oyvind Jergan</a>
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

   it should "be able to identify correct solution on board with overlap" in {
      val board = FrogPuzzleTest.testBoardWithOverlap()
      val solution = Solution.empty(board)
         .addAction(2, 1, MOVE_RIGHT)
         .addAction(3, 1, MOVE_DOWN)
         .addAction(3, 2, MOVE_LEFT)
         .addAction(1, 2, MOVE_UP)
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

   it should "be able to identify non correct solution caused by loops" in {
      val board = FrogPuzzleTest.testBoard()
      val solution = Solution.empty(board)
         .addAction(1, 3, MOVE_UP)
         .addAction(1, 2, MOVE_DOWN)
      assert(!new Simulator().correctSolution(solution))
   }

}
