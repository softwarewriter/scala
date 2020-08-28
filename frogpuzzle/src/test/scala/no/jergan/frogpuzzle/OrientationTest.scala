package no.jergan.frogpuzzle

import org.scalatest.flatspec.AnyFlatSpec

/**
 * Unit test of {@link Orientation}
 *
 * @author <a href="mailto:oyvind@jergan.no">Oyvind Jergan</a>
 * @version
 */
class OrientationTest extends AnyFlatSpec {

   behavior of "parse"

   it should "be able to parse all orientation characters" in {
      assert(Orientation.parse('u') == FACE_UP)
      assert(Orientation.parse('l') == FACE_LEFT)
      assert(Orientation.parse('d') == FACE_DOWN)
      assert(Orientation.parse('r') == FACE_RIGHT)
   }

}
