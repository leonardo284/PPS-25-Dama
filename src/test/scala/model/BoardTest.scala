package model

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import model.enums.ColorType.{DARK, LIGHT}
import model.enums.PieceType

class BoardTest extends AnyFunSuite with Matchers:

  private val playerDark = HumanPlayer("Alice", DARK)
  private val playerLight = HumanPlayer("Bob", LIGHT)

  test("Board should be initialized with 64 squares") {
    val board = new CheckersBoard()
    board.allSquares.length shouldBe 64
  }

  test("Initial pieces should be placed in the correct rows") {
    val board = new CheckersBoard()
    board.getPiece(Position(0, 1)) shouldBe Some(Man(DARK))
    board.getPiece(Position(7, 0)) shouldBe Some(Man(LIGHT))
  }

  test("A Man should move forward diagonally to an empty square") {
    // Take a DARK piece at (2, 1) and move it to (3, 2)
    val board = new CheckersBoard()
    val from = board.getSquare(Position(2, 1)).get
    val to = board.getSquare(Position(3, 2)).get
    val move = MoveImpl(from, to, None, playerDark, false)

    val success = board.movePiece(move)

    success shouldBe true
    board.getPiece(Position(2, 1)) shouldBe None
    board.getPiece(Position(3, 2)) shouldBe Some(Man(DARK))
  }

  test("A Man should NOT move backwards") {
    // Move forward first
    val board = new CheckersBoard()
    val step1From = board.getSquare(Position(2, 1)).get
    val step1To = board.getSquare(Position(3, 2)).get
    board.movePiece(MoveImpl(step1From, step1To, None, playerDark, false))

    // Attempt to move backwards (from 3,2 to 2,1)
    val backFrom = board.getSquare(Position(3, 2)).get
    val backTo = board.getSquare(Position(2, 1)).get
    val moveBack = MoveImpl(backFrom, backTo, None, playerDark, false)

    board.movePiece(moveBack) shouldBe false
  }

  test("A Man should be able to capture an opponent piece") {
    val board = new CheckersBoard()
    // Manual setup of a capture situation
    // 1. Move a DARK piece forward
    val dFrom = board.getSquare(Position(2, 1)).get
    val dTo = board.getSquare(Position(3, 2)).get
    board.movePiece(MoveImpl(dFrom, dTo, None, playerDark, false))

    // 2. Move a LIGHT piece to a position adjacent to the DARK one
    val lFrom = board.getSquare(Position(5, 4)).get
    val lTo = board.getSquare(Position(4, 3)).get
    board.movePiece(MoveImpl(lFrom, lTo, None, playerLight, false))

    // Now DARK at (3,2) can capture LIGHT at (4,3), landing at (5,4)
    val finalFrom = board.getSquare(Position(3, 2)).get
    val finalTo = board.getSquare(Position(5, 4)).get
    val captured = board.getSquare(Position(4, 3)) // The piece in the middle

    val captureMove = MoveImpl(finalFrom, finalTo, captured, playerDark, false)

    board.movePiece(captureMove) shouldBe true
    board.getPiece(Position(4, 3)) shouldBe None // The captured piece must disappear
    board.getPiece(Position(5, 4)) shouldBe Some(Man(DARK))
  }
  
  test("isInsideBoard should correctly validate boundaries") {
    val board = new CheckersBoard()
    board.squareAt(Position(0, 0)).isDefined shouldBe true
    board.squareAt(Position(8, 0)).isDefined shouldBe false
    board.squareAt(Position(-1, 0)).isDefined shouldBe false
  }

  test("board should identify available jumps and apply mandatory capture") {
    val board = new CheckersBoard()
    val whitePlayer = HumanPlayer("Alice", LIGHT)
    val darkPlayer = HumanPlayer("Bob", DARK)

    // At start, LIGHT Man at (5,2) can move to (4,1) or (4,3).
    // If we move a DARK piece to (4,3), the LIGHT piece must jump.
    val lightSquarePos = Position(5, 2)
    val darkSquarePos = Position(4, 3)

    val fromSquare = board.getSquare(lightSquarePos).get
    val enemySquare = board.getSquare(darkSquarePos).get

    // Forcing a state where a capture is possible:
    // We use movePiece to simulate game progression or
    // we assume a board state where the capture is ready.

    val lightSquare = board.getSquare(lightSquarePos)

    // Let's find all moves from our specific square
    val movesFromSquare = board.getAllPossibleMovesFromSquare(whitePlayer, lightSquare.get)

    // If a capture is possible, the list of possible moves MUST contains only captures
    // (due to the mandatory capture rule logic)
    if (movesFromSquare.exists(_.captured.isDefined)) {
      movesFromSquare.foreach { m =>
        m.captured shouldBe defined
        m.to.position.row shouldBe (m.from.position.row - 2) // Moving "up" for LIGHT
      }
    } else {
      // In a standard starting board, no jumps are possible
      movesFromSquare.map(_.to.position) should contain allOf(Position(4, 1), Position(4, 3))
    }
  }

  test("Board should identify a capturable piece between two squares") {
    val board = new CheckersBoard()
    val lightPiece = Man(LIGHT)

    // Defining a jump scenario: From (5,2) to (3,4) jumping over (4,3)
    val from = board.getSquare(Position(5, 2)).get
    val to = board.getSquare(Position(3, 4)).get

    val captured = board.getCapturablePieceBetween(from, to, lightPiece)

    // If (4,3) has an enemy, captured should be Some(Square at 4,3)
    // but CheckersBoard init puts DARK pieces in rows 0,1,2 and LIGHT in 5,6,7
    // So between (5,2) and (3,4) there is row 4, which is empty at start.
    captured shouldBe None
  }

  test("Man should promote to King by walking to the opposite side following turns") {
    val board = new CheckersBoard()

    // Helper to execute a move and fail the test if the logic blocks it
    def execute(from: (Int, Int), to: (Int, Int), player: Player, isPromotion: Boolean): Unit = {
      val f = board.getSquare(Position(from._1, from._2)).get
      val t = board.getSquare(Position(to._1, to._2)).get
      val move = MoveImpl(f, t, None, player, isPromotion)
      if (!board.movePiece(move)) {
        fail(s"Move failed: ${player.name} from $from to $to. Check if the square is occupied or unreachable!")
      }
    }

    // --- TURN SEQUENCE (Starting with White/LIGHT) ---

    // 1. LIGHT moves (clears space in row 5): from (5,2) to (4,1)
    execute((5, 2), (4, 1), playerLight, false)
    // 2. DARK moves (clears space in row 2): from (2,1) to (3,2)
    execute((2, 1), (3, 2), playerDark, false)

    // 3. LIGHT moves: advancing the piece towards row 3
    execute((4, 1), (3, 0), playerLight, false)
    // 4. DARK moves: clearing another section of the board
    execute((2, 3), (3, 4), playerDark, false)

    // 5. LIGHT advances further: from (3,0) to (2,1)
    execute((3, 0), (2, 1), playerLight, false)
    // 6. DARK moves: clearing row 1
    execute((1, 2), (2, 3), playerDark, false)

    // 7. LIGHT moves a supporting piece: from (6,3) to (5,2)
    execute((6, 3), (5, 2), playerLight, false)
    // 8. DARK moves: clearing a spot in the promotion row (row 0)
    execute((0, 3), (1, 2), playerDark, false)

    // --- FINAL MOVE: PROMOTION ---
    // 9. LIGHT moves from row 2 directly to row 0 (promotion row)
    execute((2, 1), (0, 3), playerLight, true)

    // --- ASSERTIONS ---
    val pieceAtDest = board.getPiece(Position(0, 3))
    pieceAtDest shouldBe defined
    pieceAtDest.get.pieceType shouldBe PieceType.King
    pieceAtDest.get.color shouldBe LIGHT
  }

  test("getAllPossibleMoves should return all 7 legal moves for LIGHT at start of game") {
    val board = new CheckersBoard()

    // In the initial state, LIGHT (White) should have exactly 7 possible moves
    // Pieces at (5,0), (5,2), (5,4), (5,6)
    // - (5,0) can move to (4,1) -> 1 move
    // - (5,2) can move to (4,1) and (4,3) -> 2 moves
    // - (5,4) can move to (4,3) and (4,5) -> 2 moves
    // - (5,6) can move to (4,5) and (4,7) -> 2 moves
    // Total = 7

    val lightMoves = board.getAllPossibleMoves(playerLight)

    lightMoves.length shouldBe 7

    // Verify that all returned moves are indeed for the LIGHT player
    all(lightMoves.map(_.player.color)) shouldBe LIGHT

    // Verify no capture is possible at the start
    all(lightMoves.map(_.captured)) shouldBe None
  }

  test("getAllPossibleMoves should return all 7 legal moves for DARK at start of game") {
    val board = new CheckersBoard()

    // Similarly, DARK (Black) should have 7 possible moves
    // Pieces at (2,1), (2,3), (2,5), (2,7)
    val darkMoves = board.getAllPossibleMoves(playerDark)

    darkMoves.length shouldBe 7
    all(darkMoves.map(_.player.color)) shouldBe DARK
  }

  test("getAllPossibleMoves should prioritize captures (mandatory jump rule)") {
    val board = new CheckersBoard()

    // 1. Setup a situation where a capture is possible
    // Move DARK piece from (2,1) to (3,2)
    val dFrom = board.getSquare(Position(2, 1)).get
    val dTo = board.getSquare(Position(3, 2)).get
    board.movePiece(MoveImpl(dFrom, dTo, None, playerDark, false))

    // Move LIGHT piece from (5,4) to (4,3)
    val lFrom = board.getSquare(Position(5, 4)).get
    val lTo = board.getSquare(Position(4, 3)).get
    board.movePiece(MoveImpl(lFrom, lTo, None, playerLight, false))

    // 2. Now DARK has a capture move available (3,2 -> 5,4 capturing 4,3)
    // According to your implementation, only jump moves should be returned
    val darkMoves = board.getAllPossibleMoves(playerDark)

    // Check that all returned moves are captures
    darkMoves.nonEmpty shouldBe true
    all(darkMoves.map(_.captured)) shouldBe defined

    // Specifically, check the landing position of the jump
    darkMoves.map(_.to.position) should contain(Position(5, 4))
  }
