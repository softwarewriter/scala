package no.jergan.frogpuzzle

import org.scalatest.flatspec.AnyFlatSpec

/**
 * Unit test of {@link Board}
 *
 * @author <a href="mailto:oyvind@jergan.no">Oyvind Jergan</a>
 * @version
 */
class BoardTest extends AnyFlatSpec {

  behavior of "parse"

  it should "be able to parse board from string" in {
    val boardAsString = io.Source.fromInputStream(getClass.getResourceAsStream("/testboard2.txt")).mkString
    val board = Board.parse(boardAsString)
    println(board)

    assert(board.sizeX == 5);
    assert(board.sizeY == 6);

    assert(board.start() == Position(1, 4))
    assert(board.end() == Position(3, 1))
    //        println(board.asString());

//    Assertions.assertEquals(Position(1, 4), board.initialState().position);
//    Assertions.assertEquals(Orientation.UP, board.initialState().orientation);

  }

}
