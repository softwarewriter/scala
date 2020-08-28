package no.jergan.frogpuzzle

/**
 * Utility methods for testing.
 *
 * @author <a href="mailto:oyvind@jergan.no">Oyvind Jergan</a>
 */
object FrogPuzzleTest {

   def testBoard(): Board = {
      board("/testboard.txt")
   }

   def testBoardWithOverlap(): Board = {
      board("/testboardWithOverlap.txt")
   }

   private[this] def board(filename: String): Board = {
      val boardAsString = io.Source.fromInputStream(getClass.getResourceAsStream(filename)).mkString
      Board.parse(boardAsString)
   }

}
