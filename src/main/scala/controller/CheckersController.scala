package controller

import model.{GameImpl, Game, Move, Piece, MoveImpl, Player, Position, Square}
import view.GamePage


class CheckersController(game: GameImpl, view : GamePage) extends GameController(game, view) :

  private var selected: Option[Square] = None

  /**
   * Sets the specified square as the current selection and highlights its valid moves.
   * This method calculates all legal destinations for the piece on the square
   * and triggers the view to provide visual feedback to the player.
   *
   * @param square the square containing the piece to be selected.
   */
  private def selectSquare(square: Square) =
    selected = Some(square)
    val moves = game.currentBoard.possibleMoves(square)
    println(moves)
    val movePositions = moves.map(_.position)
    view.highlightSquares(movePositions)
    println(s"Selezionata pedina in ${square.position}")

  /**
   * Clears the current selection and restores the board's original visual state.
   * This method resets any active highlights and re-renders the board to ensure
   * the UI is clean and synchronized with the current model state.
   */
  private def deselectSquare() =
    selected = None
    view.resetBoardColors()
    view.render(game.currentBoard)

  /**
   * Handles the event when a square on the board is clicked.
   *
   * @param pos the coordinates of the clicked square.
   */
  override def onSquareClicked(pos: Position): Unit =
    val clickedSquareOpt = game.currentBoard.squareAt(pos)
  
    selected match {
      // a square has been already selected
      case Some(fromSquare) =>
        clickedSquareOpt match {
          // CASE 1: Clicked on a piece belonging to the current player (Switch or Deselect)
          case Some(newSquare) if newSquare.piece.exists(_.color == game.currentTurn.color) =>
            if (fromSquare.position == pos) {
              deselectSquare()
            } else {
              // Change selection and update highlighted move indicators
              selectSquare(newSquare)
            }
  
          // CASE 2: Clicked on an EMPTY or ENEMY cell (Move attempt)
          case Some(toSquare) =>
            // Validate if the move is within the possible moves calculated by the board logic
            val possibleMoves = game.currentBoard.possibleMoves(fromSquare)
  
            if (possibleMoves.exists(_.position == pos)) {
              // Target cell is valid: construct and execute the move
              val movingPiece = fromSquare.piece.get
              val move = MoveImpl(
                from = fromSquare,
                to = toSquare,
                captured = game.currentBoard.getCapturablePieceBetween(fromSquare, toSquare, movingPiece),
                player = game.currentTurn,
                isPromotion = false
              )
              makeMove(move)
              deselectSquare() // Clear highlights after successful move
            } else {
              // Clicked on an invalid cell: reset selection
              deselectSquare()
            }
  
          case None => deselectSquare()
        }
  
      case None =>
        // CASE 3: No previous selection exists
        clickedSquareOpt match {
          case Some(square) if square.piece.exists(_.color == game.currentTurn.color) =>
            selectSquare(square)
          case _ =>
            // Feedback for invalid initial selection
            view.logError("Please select one of your pieces")
        }
    }

