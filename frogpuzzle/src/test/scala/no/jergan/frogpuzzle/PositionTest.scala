package no.jergan.frogpuzzle

import org.scalatest.flatspec.AnyFlatSpec

/**
 * Unit test of [[Position]].
 *
 * @author <a href="mailto:oyvind@jergan.no">Oyvind Jergan</a>
 */
class PositionTest extends AnyFlatSpec {

   behavior of "move"

   it should "be able to move without action" in {
      assert(Position(2, 2).move(None, FACE_UP).equals(Position(2, 1)))
      assert(Position(2, 2).move(None, FACE_RIGHT).equals(Position(3, 2)))
      assert(Position(2, 2).move(None, FACE_DOWN).equals(Position(2, 3)))
      assert(Position(2, 2).move(None, FACE_LEFT).equals(Position(1, 2)))
   }

   it should "be able to move with action" in {
      assert(Position(2, 2).move(Some(MOVE_UP), FACE_LEFT).equals(Position(2, 1)))
      assert(Position(2, 2).move(Some(MOVE_RIGHT), FACE_UP).equals(Position(3, 2)))
      assert(Position(2, 2).move(Some(MOVE_DOWN), FACE_RIGHT).equals(Position(2, 3)))
      assert(Position(2, 2).move(Some(MOVE_LEFT), FACE_DOWN).equals(Position(1, 2)))
      assert(Position(2, 2).move(Some(JUMP), FACE_DOWN).equals(Position(2, 4)))
   }

}
