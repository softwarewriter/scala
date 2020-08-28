package no.jergan.frogpuzzle

import org.scalatest.flatspec.AnyFlatSpec

/**
 * Unit test of [[Action]].
 *
 * @author <a href="mailto:oyvind@jergan.no">Oyvind Jergan</a>
 */
class ActionTest extends AnyFlatSpec {

   behavior of "parse"

   it should "be able to parse all action characters" in {
      assert(Action.parse('u') == MOVE_UP)
      assert(Action.parse('l') == MOVE_LEFT)
      assert(Action.parse('d') == MOVE_DOWN)
      assert(Action.parse('r') == MOVE_RIGHT)
   }

}
