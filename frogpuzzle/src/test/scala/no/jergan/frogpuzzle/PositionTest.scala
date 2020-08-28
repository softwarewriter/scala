package no.jergan.frogpuzzle

import org.scalatest.flatspec.AnyFlatSpec

/**
 * Unit test of {@link Position}
 *
 * @author <a href="mailto:oyvind@jergan.no">Oyvind Jergan</a>
 * @version
 */
class PositionTest extends AnyFlatSpec {

  behavior of "move"

  it should "be able to move without action" in {
    val newPosition = new Position(2, 2).move(None, FACE_UP);
    println(newPosition);
    assert(new Position(2, 2).move(None, FACE_UP).equals(new Position(2, 1)))
  }

}
