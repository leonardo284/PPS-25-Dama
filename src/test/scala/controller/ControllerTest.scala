package controller

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import model.{Board, Game, NotYourPiece, Position}
import model.enums.ColorType.{DARK, LIGHT}
import view.GamePage

class ControllerTest extends AnyFunSuite with Matchers:

  /**
   * Mock implementation of the GamePage to intercept and verify
   * calls made by the Controller without launching a GUI.
   */
  class MockView extends GamePage:
    var lastBoardRendered: Option[Board] = None
    var currentHighlights: List[Position] = Nil
    var lastError: String = ""
    var winner: String = ""
    var isInputEnabled: Boolean = true

    override def setController(c: GameController): Unit = ()
    override def show(): Unit = ()
    override def render(board: Board): Unit = lastBoardRendered = Some(board)
    override def highlightSquares(positions: List[Position]): Unit = currentHighlights = positions
    override def logError(errorMsg: String): Unit = lastError = errorMsg
    override def initializeChecker(): Unit = ()
    override def resetBoardColors(): Unit = currentHighlights = Nil
    override def disableInput(): Unit = isInputEnabled = false
    override def enableInput(): Unit = isInputEnabled = true
    override def showWinner(winnerName: String): Unit = winner = winnerName

  test("Controller should highlight valid moves when a player's piece is selected") {
    val game = Game("Alice", "Bob")
    val view = new MockView()
    val controller = new CheckersController(game, view)

    // LIGHT (Alice) starts. Select a piece at row 5, col 0
    val startPos = Position(5, 0)
    controller.onSquareClicked(startPos)

    // Verify the view received positions to highlight (e.g., 4, 1 is a valid move)
    view.currentHighlights should not be empty
    view.currentHighlights should contain (Position(4, 1))
  }

  test("Controller should deselect and clear highlights when the same piece is clicked twice") {
    val game = Game("Alice", "Bob")
    val view = new MockView()
    val controller = new CheckersController(game, view)
    val pos = Position(5, 0)

    controller.onSquareClicked(pos) // First click: selection
    view.currentHighlights should not be empty

    controller.onSquareClicked(pos) // Second click: deselection
    view.currentHighlights shouldBe empty
  }

  test("Controller should execute a move and refresh the view when clicking a valid target") {
    val game = Game("Alice", "Bob")
    val view = new MockView()
    val controller = new CheckersController(game, view)

    val from = Position(5, 0)
    val to = Position(4, 1)

    controller.onSquareClicked(from) // Select piece
    controller.onSquareClicked(to)   // Perform move

    // Verify model state and view refresh
    game.getMoves should have size 1
    view.lastBoardRendered shouldBe Some(game.currentBoard)
    view.currentHighlights shouldBe empty // Highlights should be cleared after move
  }

  test("Controller should display an error message when clicking an invalid square initially") {
    val game = Game("Alice", "Bob")
    val view = new MockView()
    val controller = new CheckersController(game, view)

    // Click on an empty center square (3, 3) without selecting a piece first
    controller.onSquareClicked(Position(3, 3))

    view.lastError shouldBe NotYourPiece.message
  }

  test("undoMove should revert the move in the model and update the view") {
    val game = Game("Alice", "Bob")
    val view = new MockView()
    val controller = new CheckersController(game, view)

    // Perform a quick move sequence
    controller.onSquareClicked(Position(5, 0))
    controller.onSquareClicked(Position(4, 1))

    val historySizeBeforeUndo = game.getMoves.size
    controller.undoMove()

    // Verify move was popped from history and view was notified
    game.getMoves.size shouldBe (historySizeBeforeUndo - 1)
    view.lastBoardRendered shouldBe Some(game.currentBoard)
  }

  test("canUndo should return false in PvAI mode if only one move has been played") {
    // Single parameter constructor initializes PvAI mode
    val game = Game("HumanPlayer")
    val view = new MockView()
    val controller = new CheckersController(game, view)

    // In PvAI, at least 2 moves are required (Player + AI response) to undo safely
    controller.canUndo shouldBe false
  }

  test("disableInput should be reflected in the view state") {
    val game = Game("Alice", "Bob")
    val view = new MockView()
    val controller = new CheckersController(game, view)

    // Simulating an action that triggers input disabling (like start of AI turn)
    view.disableInput()

    view.isInputEnabled shouldBe false
  }