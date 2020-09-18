package no.jergan.frogpuzzle

import org.scalatest._

/**
 * Unit test of [[Board]].
 *
 * @author <a href="mailto:oyvind@jergan.no">Oyvind Jergan</a>
 */
class BoardTest extends FlatSpec {

   behavior of "parse"

   it should "be able to parse board from string" in {
      val board = FrogPuzzleTest.testBoard().getOrElse(fail())

      assert(board.sizeX() == 3)
      assert(board.sizeY() == 4)

      assert(board.start == Position(0, 3))
      assert(board.end == Position(2, 0))

      assert(board.initialState().position == Position(0, 3))
      assert(board.initialState().orientation == FACE_UP)
   }

}
