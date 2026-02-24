package controller

import model.{Game, NotYourPiece, Position, Square}
import view.GamePage


class CheckersController(game: Game, view : GamePage) extends GameController(game, view) :

  private var selected: Option[Square] = None

  /**
   * Sets the specified square as the current selection and highlights its valid moves.
   * This method calculates all legal destinations for the piece on the square
   * and triggers the view to provide visual feedback to the player.
   *
   * @param square the square containing the piece to be selected.
   */
  private def selectSquare(square: Square): Unit =
    selected = Some(square)
    val moves = game.currentBoard.getAllPossibleMovesFromSquare(currentPlayer, square)
    view.highlightSquares(moves.map(_.to.position))

  /**
   * Clears the current selection and restores the board's original visual state.
   * This method resets any active highlights and re-renders the board to ensure
   * the UI is clean and synchronized with the current model state.
   */
  private def deselectSquare(): Unit =
    selected = None
    view.resetBoardColors()
    view.render(game.currentBoard)

  /**
   * Handles the event when a square on the board is clicked.
   *
   * @param pos the coordinates of the clicked square.
   */
  override def onSquareClicked(pos: Position): Unit =
    val clickedSquareOpt = game.currentBoard.getSquare(pos)
    val currentTurnColor = game.currentTurn.color

    (selected, clickedSquareOpt) match
      // CASE 1: Clicked on the same square already selected -> Deselect
      case (Some(from), Some(to)) if from.position == to.position => deselectSquare()

      // CASE 2: Clicked on another player's own piece -> Change selection
      case (_, Some(to)) if to.piece.exists(_.color == currentTurnColor) => selectSquare(to)

      // CASE 3: A piece was selected and clicking elsewhere -> Attempt a move
      case (Some(from), Some(to)) => handleMoveAttempt(from, to)

      // CASE 4: No selection and clicked on something that doesn't belong to the player
      case _ =>
        deselectSquare()
        view.logError(NotYourPiece.message)


  private def handleMoveAttempt(from: Square, to: Square): Unit =
    val possibleMoves = game.currentBoard.getAllPossibleMovesFromSquare(currentPlayer, from)

    // Check if there is a legal move that lands on 'to'
    possibleMoves.find(_.to.position == to.position) match
      case Some(move) =>
        makeMove(move)
        deselectSquare()
      case None =>
        deselectSquare()