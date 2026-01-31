package model

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import model.enums.ColorType.{DARK, LIGHT}
import org.scalatest.EitherValues._

class GameTest extends AnyFunSuite with Matchers:
  test("Game should initialize players with different colors") {
    val game = Game("Alice", "Bob")
    val (p1, p2) = game.getPlayers

    // Ensure both colors are assigned across the two players
    p1.color should not be p2.color
    List(p1.color, p2.color) should contain allOf(DARK, LIGHT)
  }

  test("Game should correctly identify the starting player (LIGHT)") {
    val game = Game("Alice", "Bob")
    val (p1, p2) = game.getPlayers

    // Light usually starts; verify currentTurn matches the player with LIGHT color
    val startingPlayer = if (p1.color == LIGHT) p1 else p2
    game.currentTurn shouldBe startingPlayer
  }

  test("makeMove should fail if a player tries to move out of turn") {
    val game = Game("Alice", "Bob")
    val (p1, p2) = game.getPlayers

    // Identify the player who is NOT currently active
    val waitingPlayer = if (game.currentTurn == p1) p2 else p1

    // Setup a move for the inactive player
    val from = game.currentBoard.getSquare(Position(2, 1)).get
    val to = game.currentBoard.getSquare(Position(3, 2)).get
    val move = MoveImpl(from, to, None, waitingPlayer, false)

    val result = game.makeMove(move)

    // The result should be a Left (failure) regardless of the error message
    result.isLeft shouldBe true

    // Turn should remain unchanged after a failed move attempt
    game.currentTurn should not be waitingPlayer
  }

  test("Successful makeMove should update history and switch turn") {
    val game = Game("Alice", "Bob")
    val initialTurn = game.currentTurn

    // Select coordinates based on the current player's side
    val fromPos = if (initialTurn.color == LIGHT) Position(5, 0) else Position(2, 1)
    val toPos = if (initialTurn.color == LIGHT) Position(4, 1) else Position(3, 2)

    val from = game.currentBoard.getSquare(fromPos).get
    val to = game.currentBoard.getSquare(toPos).get
    val move = MoveImpl(from, to, None, initialTurn, false)

    val result = game.makeMove(move)

    // Verify move was successful and turn state was updated
    result.isRight shouldBe true
    game.getMoves should have size 1
    game.currentTurn should not be initialTurn
  }

  test("undoMove should restore the previous turn and clean history") {
    val game = Game("Alice", "Bob")
    val initialTurn = game.currentTurn
    // Execute a standard opening move
    val fromPos = if (initialTurn.color == LIGHT) Position(5, 0) else Position(2, 1)
    val toPos = if (initialTurn.color == LIGHT) Position(4, 1) else Position(3, 2)
    val move = MoveImpl(game.currentBoard.getSquare(fromPos).get, game.currentBoard.getSquare(toPos).get, None, initialTurn, false)

    game.makeMove(move)

    game.getMoves.length shouldBe 1
    
    // Revert the last action
    val success = game.undoMove()

    success shouldBe true
    game.getMoves shouldBe empty
    game.currentTurn shouldBe initialTurn
  }

  test("undoMove should return false when no moves have been played") {
    val game = Game("Alice", "Bob")
    game.undoMove() shouldBe false
  }

  test("makeAIMove should correctly identify if it is AI turn based on assigned color") {
    val game = Game("HumanPlayer") // PvAI mode
    val (p1, p2) = game.getPlayers
    val aiPlayer = if (p1.isInstanceOf[AIPlayer]) p1 else p2

    // If AI is LIGHT, it must be its turn immediately
    if (aiPlayer.color == LIGHT) {
      game.isAITurn shouldBe true
    } else {
      game.isAITurn shouldBe false
    }
  }
