package controller

import model.{Game, Move, Player, Position, Square}
import view.GamePage

/**
 * The controller that coordinates the game logic (Model) and the user interface (View).
 * It handles user inputs, updates the game state, and refreshes the display.
 * @param game the game logic implementation.
 * @param view the graphical interface page.
 */
trait GameController(val game: Game, view : GamePage) :

  /**
   * Handles the event when a square on the board is clicked.
   *
   * @param pos the coordinates of the clicked square.
   */
  def onSquareClicked(pos: Position): Unit

  /**
   * Provides an iterator to traverse all positions and squares of the current board.
   * Useful for initializing or bulk-updating the UI components.
   */
  def iteratorWithPositions: Iterator[(Position, Square)] = game.currentBoard.iteratorWithPositions

  /**
   * Retrieves a specific square from the board at the given position.
   *
   * @param pos the coordinates of the square to retrieve.
   * @return an Option containing the Square if it exists, None otherwise.
   */
  def getSquare(pos: Position): Option[Square] = game.currentBoard.getSquare(pos)

  /**
   * Initializes and displays the game view.
   * Sets up the link between view and controller, creates the board, and renders the initial state.
   */
  def startView(): Unit =
    view.setController(this)
    view.initializeChecker()
    view.show()
    view.render(game.currentBoard)

  /**
   * Attempts to execute a move in the game model.
   * If successful, the view is updated; otherwise, an error is logged.
   *
   * @param move the move to be processed.
   */
  def makeMove(move: Move): Unit =
    game.makeMove(move) match {
      case Right(_) => view.render(game.currentBoard)
      case Left(err) => view.logError("Mossa non valida!")
    }

  /**
   * Retrieves the Player object whose turn it is currently.
   *
   * @return the Player instance currently in turn.
   */
  def currentPlayer: Player = game.currentTurn

  /**
   * Retrieves the two players participating in the current game session.
   * @return a tuple containing (Player 1, Player 2)
   */
  def players: (Player, Player) = game.getPlayers
