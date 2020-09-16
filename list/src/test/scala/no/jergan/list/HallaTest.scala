package no.jergan.list

import org.scalatest.FunSuite

/**
 * What does this class do?
 *
 * @author <a href="mailto:oyvind@jergan.no">Oyvind Jergan</a>
 */
object HallaTest {

}

class HallaTest extends FunSuite {
   test("Halla.hei") {
      assert(Halla.hei() === 42)
   }
}
