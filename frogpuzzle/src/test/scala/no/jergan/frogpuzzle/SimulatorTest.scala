package no.jergan.frogpuzzle

import org.scalatest._

/**
 * Unit test of [[Simulator]].
 *
 * @author <a href="mailto:oyvind@jergan.no">Oyvind Jergan</a>
 */
class SimulatorTest extends FlatSpec {

   behavior of "correctSolution"

   it should "be able to identify correct solution" in {
      val board = FrogPuzzleTest.testBoard().getOrElse(fail())
      val solution = Solution.build(board,
         (0, 1, MOVE_RIGHT),
         (1, 1, MOVE_DOWN),
         (1, 2, MOVE_RIGHT),
         (2, 2, MOVE_UP)).getOrElse(fail())
      assert(new Simulator().correctSolution(solution))
   }

   it should "be able to identify correct solution on board with overlap" in {
      val board = FrogPuzzleTest.testBoardWithOverlap().getOrElse(fail())
      val solution = Solution.build(board,
         (1, 0, MOVE_RIGHT),
         (2, 0, MOVE_DOWN),
         (2, 1, MOVE_LEFT),
         (0, 1, MOVE_UP)).getOrElse(fail())
      assert(new Simulator().correctSolution(solution))
   }

   it should "be able to identify correct solution on board that requires jump" in {
      val board = FrogPuzzleTest.testBoardThatRequiresJump().getOrElse(fail())
      val solution = Solution.build(board,
         (1, 2, JUMP),
         (1, 0, MOVE_LEFT),
         (0, 0, MOVE_DOWN),
         (0, 1, MOVE_RIGHT),
         (1, 1, JUMP)).getOrElse(fail())
      assert(new Simulator().correctSolution(solution))
   }

   it should "be able to identify correct solution on board with portal" in {
      val board = FrogPuzzleTest.testBoardWithPortal().getOrElse(fail())
      val solution = Solution.build(board,
         (1, 1, MOVE_RIGHT),
         (1, 2, MOVE_UP),
         (2, 2, MOVE_LEFT),
         (0, 3, MOVE_DOWN),
         (1, 3, MOVE_LEFT),
         (2, 4, MOVE_UP)).getOrElse(fail())
      assert(new Simulator().correctSolution(solution))
   }

   it should "be able to identify non correct solution" in {
      val board = FrogPuzzleTest.testBoard().getOrElse(fail())
      val solution = Solution.build(board,
         (0, 1, MOVE_RIGHT),
         (1, 1, MOVE_DOWN),
         (1, 2, MOVE_RIGHT)).getOrElse(fail())
      assert(!new Simulator().correctSolution(solution))
   }

   it should "be able to identify non correct solution caused by loops" in {
      val board = FrogPuzzleTest.testBoard().getOrElse(fail())
      val solution = Solution.build(board,
         (0, 2, MOVE_UP),
         (0, 1, MOVE_DOWN)).getOrElse(fail())
      assert(!new Simulator().correctSolution(solution))
   }

}
