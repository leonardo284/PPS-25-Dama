package model

import model.enums.{ColorType, PieceType}
import model.enums.ColorType.{DARK, LIGHT}


/**
 * Interface representing the checkerboard.
 */
trait Board:

  /**
   * Retrieves the piece at a specific position.
   * @param pos the board position to check.
   * @return an Option containing the Piece if present, None otherwise.
   */
  def getPiece(pos: Position): Option[Piece]

  /**
   * Retrieves the square object at a specific position.
   * @param pos the board position to check.
   * @return an Option containing the Square if within bounds, None otherwise.
   */
  def getSquare(pos: Position): Option[Square]

  /**
   * Returns the square at a given position.
   * @param pos the board's position.
   * @return an optional square if the position is inside the board.
   */
  def squareAt(pos: Position): Option[Square]

  /**
   * Calculates all legal moves (both normal moves and captures) for a given square.
   * @param from the starting square.
   * @return a list of possible destination squares.
   */
  def possibleMoves(from: Square): List[Square]

  /**
   * Executes a piece movement on the board.
   * @param move the move object containing source, destination, and potential captured pieces.
   * @return true if the move was valid and successfully applied, false otherwise.
   */
  def movePiece(move: Move): Boolean

  /**
   * Returns an iterator over all squares on the board.
   * @return an iterator of Square objects.
   */
  def iterator: Iterator[Square]

  /**
   * Returns a flat sequence of all squares.
   * @return a Seq containing all Square objects.
   */
  def allSquares: Seq[Square]

  /**
   * Returns an iterator that provides pairs of positions and their corresponding squares.
   * @return an iterator of (Position, Square) tuples.
   */
  def iteratorWithPositions: Iterator[(Position, Square)]

  /**
   * Provides a 2D matrix view of the board's squares.
   * @return a Vector of Vectors representing the board grid.
   */
  def squaresView: Vector[Vector[Square]]

  /**
   * Identifies the square containing a captured piece between the start and end of a move.
   * @param move the move performed.
   * @return an Option containing the captured Square, if one exists.
   */
  def getCapturablePieceBetween(move: Move): Option[Square]


  /**
   * Companion object for Board.
   */
  object Board {
    val Size: Int = 8
    val PieceRowNumber: Int = 3
  }