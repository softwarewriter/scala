package no.jergan.frogpuzzle

import org.scalatest.flatspec.AnyFlatSpec

/**
 * Unit test of {@link Action}
 *
 * @author <a href="mailto:oyvind@jergan.no">Oyvind Jergan</a>
 * @version
 */
class ActionTest extends AnyFlatSpec {

  behavior of "parse"

  it should "be able to parse all action characters" in {
    assert(Action.parse('u') == UP)
    assert(Action.parse('l') == LEFT)
    assert(Action.parse('d') == DOWN)
    assert(Action.parse('r') == RIGHT)
  }

}
