package model

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import model.enums.ColorType.{DARK, LIGHT}
import model.enums.PieceType

class BoardTest extends AnyFunSuite with Matchers {

  private val board: Board = new CheckersBoard()
  private val playerDark = Player("Alice", DARK)
  private val playerLight = Player("Bob", LIGHT)

  test("Board should be initialized with 64 squares") {
    board.allSquares.length shouldBe 64
  }

  test("Initial pieces should be placed in the correct rows") {
    board.getPiece(Position(0, 1)) shouldBe Some(Man(DARK))
    board.getPiece(Position(7, 0)) shouldBe Some(Man(LIGHT))
  }

  test("A Man should move forward diagonally to an empty square") {
    // Take a DARK piece at (2, 1) and move it to (3, 2)
    val from = board.getSquare(Position(2, 1)).get
    val to = board.getSquare(Position(3, 2)).get
    val move = MoveImpl(from, to, None, playerDark)

    val success = board.movePiece(move)

    success shouldBe true
    board.getPiece(Position(2, 1)) shouldBe None
    board.getPiece(Position(3, 2)) shouldBe Some(Man(DARK))
  }

  test("A Man should NOT move backwards") {
    // Move forward first
    val step1From = board.getSquare(Position(2, 1)).get
    val step1To = board.getSquare(Position(3, 2)).get
    board.movePiece(MoveImpl(step1From, step1To, None, playerDark))

    // Attempt to move backwards (from 3,2 to 2,1)
    val backFrom = board.getSquare(Position(3, 2)).get
    val backTo = board.getSquare(Position(2, 1)).get
    val moveBack = MoveImpl(backFrom, backTo, None, playerDark)

    board.movePiece(moveBack) shouldBe false
  }

  test("A Man should be able to capture an opponent piece") {

    // Manual setup of a capture situation
    // 1. Move a DARK piece forward
    val dFrom = board.getSquare(Position(2, 1)).get
    val dTo = board.getSquare(Position(3, 2)).get
    board.movePiece(MoveImpl(dFrom, dTo, None, playerDark))

    // 2. Move a LIGHT piece to a position adjacent to the DARK one
    val lFrom = board.getSquare(Position(5, 4)).get
    val lTo = board.getSquare(Position(4, 3)).get
    board.movePiece(MoveImpl(lFrom, lTo, None, playerLight))

    // Now DARK at (3,2) can capture LIGHT at (4,3), landing at (5,4)
    val finalFrom = board.getSquare(Position(3, 2)).get
    val finalTo = board.getSquare(Position(5, 4)).get
    val captured = board.getSquare(Position(4, 3)) // The piece in the middle

    val captureMove = MoveImpl(finalFrom, finalTo, captured, playerDark)

    board.movePiece(captureMove) shouldBe true
    board.getPiece(Position(4, 3)) shouldBe None // The captured piece must disappear
    board.getPiece(Position(5, 4)) shouldBe Some(Man(DARK))
  }
  
  test("isInsideBoard should correctly validate boundaries") {
    board.squareAt(Position(0, 0)).isDefined shouldBe true
    board.squareAt(Position(8, 0)).isDefined shouldBe false
    board.squareAt(Position(-1, 0)).isDefined shouldBe false
  }

  test("possibleMoves should identify available jumps") {

    // Setup for a piece that can only perform normal moves/captures
    val square = board.getSquare(Position(2, 1)).get
    val moves = board.possibleMoves(square)

    // At the start, a DARK Man at (2,1) should have moves to (3,0) and (3,2)
    moves.map(_.position) should contain allOf (Position(3, 0), Position(3, 2))
  }
}