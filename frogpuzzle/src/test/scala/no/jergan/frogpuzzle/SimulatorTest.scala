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
      val board = FrogPuzzleTest.testBoard().getOrElse(fail())
      val solution = Solution.build(board,
         (1, 2, MOVE_RIGHT),
         (2, 2, MOVE_DOWN),
         (2, 3, MOVE_RIGHT),
         (3, 3, MOVE_UP)).getOrElse(fail())
      assert(new Simulator().correctSolution(solution))
   }

   it should "be able to identify correct solution on board with overlap" in {
      val board = FrogPuzzleTest.testBoardWithOverlap().getOrElse(fail())
      val solution = Solution.build(board,
         (2, 1, MOVE_RIGHT),
         (3, 1, MOVE_DOWN),
         (3, 2, MOVE_LEFT),
         (1, 2, MOVE_UP)).getOrElse(fail())
      assert(new Simulator().correctSolution(solution))
   }

   it should "be able to identify non correct solution" in {
      val board = FrogPuzzleTest.testBoard().getOrElse(fail())
      val solution = Solution.build(board,
         (1, 2, MOVE_RIGHT),
         (2, 2, MOVE_DOWN),
         (2, 3, MOVE_RIGHT)).getOrElse(fail())
      assert(!new Simulator().correctSolution(solution))
   }

   it should "be able to identify non correct solution caused by loops" in {
      val board = FrogPuzzleTest.testBoard().getOrElse(fail())
      val solution = Solution.build(board,
         (1, 3, MOVE_UP),
         (1, 2, MOVE_DOWN)).getOrElse(fail())
      assert(!new Simulator().correctSolution(solution))
   }

}
