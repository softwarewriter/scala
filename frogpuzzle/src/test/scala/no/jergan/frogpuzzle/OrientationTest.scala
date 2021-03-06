package no.jergan.frogpuzzle

import org.scalatest._

/**
 * Unit test of [[Orientation]].
 *
 * @author <a href="mailto:oyvind@jergan.no">Oyvind Jergan</a>
 */
class OrientationTest extends FlatSpec {

   behavior of "parse"

   it should "be able to parse all orientation characters" in {
      assert(Orientation.parse('u') == FACE_UP)
      assert(Orientation.parse('l') == FACE_LEFT)
      assert(Orientation.parse('d') == FACE_DOWN)
      assert(Orientation.parse('r') == FACE_RIGHT)
   }
}
