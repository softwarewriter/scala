package no.jergan.frogpuzzle

/**
 * Utility methods for testing.
 *
 * @author <a href="mailto:oyvind@jergan.no">Oyvind Jergan</a>
 */
object FrogPuzzleTest {

   def testBoard(): Either[String, Board] = {
      board("/testboard.txt")
   }

   def testBoardWithOverlap(): Either[String, Board] = {
      board("/testboardWithOverlap.txt")
   }

   private[this] def board(filename: String): Either[String, Board] = {
      val boardAsString = io.Source.fromInputStream(getClass.getResourceAsStream(filename)).mkString
      Board.parse(boardAsString)
   }

}
