package no.jergan.frogpuzzle

/**
 * Utility methods for testing.
 *
 * @author <a href="mailto:oyvind@jergan.no">Oyvind Jergan</a>
 */
object FrogPuzzleTest {

   def testBoard(): Board = {
      val boardAsString = io.Source.fromInputStream(getClass.getResourceAsStream("/testboard.txt")).mkString
      Board.parse(boardAsString)
   }

}
