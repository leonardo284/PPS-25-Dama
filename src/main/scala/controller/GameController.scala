package controller

import model.enums.GameType.PvAI
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

    if(game.isAITurn)
      makeAIMove()


  /**
   * Attempts to execute a move in the game model.
   * If successful, the view is updated; otherwise, an error is logged.
   *
   * @param move the move to be processed.
   */
  def makeMove(move: Move): Unit =
    game.makeMove(move) match {
      case Right(_) =>
        view.render(game.currentBoard)
        if(game.isAITurn)
          makeAIMove()

        // After each move, check if the game has ended
        if(game.isGameFinished)
          val winner = game.getWinner
          view.showWinner(winner.get.name)

      case Left(err) => view.logError("Mossa non valida!")
    }


  /**
   * Handles the AI's turn by executing its move logic in a background thread.
   * Simulates a "thinking" delay for better UX, and then updates the game state.
   */
  private def makeAIMove(): Unit =
    view.disableInput()
    new Thread(() => {
      try {
        Thread.sleep(1000)    // Wait for 1 second to simulate AI thinking time

        game.makeAIMove()

        javax.swing.SwingUtilities.invokeLater(() => {
          view.render(game.currentBoard)
          view.enableInput()
        })
      } catch {
        case e: InterruptedException => Thread.currentThread().interrupt()
      }
    }).start()

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

  /**
   * Undoes the last move(s) performed in the game.
   * If playing against AI, it undoes both the AI move and the player's last move.
   */
  def undoMove(): Unit =
    if game.getMoves.nonEmpty then
      if game.isAITurn then
        // If it is the AI's turn (thinking phase), undo is disabled to prevent state conflicts
        view.logError("Attendi il turno dell'IA prima di annullare")
      else
        // In PvAI mode, we must revert two moves (AI's response + Player's move)
        // to allow the user to retake their turn from the correct state.
        view.resetBoardColors()
        if game.selectedMode == PvAI then
          game.undoMove() // Undo AI move
          game.undoMove() // Undo Player move
        else
          game.undoMove() // In PvP mode, undo only the single last move

        view.render(game.currentBoard)
        view.logError("Mossa annullata")
    else
      view.logError("Nessuna mossa da annullare")

  /**
   * Checks if an undo operation is currently possible.
   * For PvAI games, at least two moves are required to return to the player's turn.
   * * @return true if the undo conditions are met, false otherwise.
   */
  def canUndo: Boolean =
    if game.selectedMode == PvAI then {
      game.getMoves.size >= 2
    } else
      game.getMoves.nonEmpty