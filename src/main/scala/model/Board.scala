package model

import enums.ColorType

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
   * Overloaded version to find a capturable piece between two squares for a specific piece.
   *
   * @param from        the starting square.
   * @param to          the destination square.
   * @param movingPiece the piece performing the move.
   * @return an Option containing the square with the piece to be captured.
   */
  def getCapturablePieceBetween(from: Square, to: Square, movingPiece: Piece): Option[Square]

/**
 * Represents the checkers board.
 */
class CheckersBoard extends Board:

  /**
   * Checks if a given position is within the board's dimensions.
   *
   * @param pos the position to validate.
   * @return true if the position is inside the grid, false otherwise.
   */
  private def isInsideBoard(pos: Position): Boolean = pos.row >= 0 && pos.row < squares.length &&
    pos.col >= 0 && pos.col < squares(pos.row).length

  /**
   * Determines the color of a square based on its row and column coordinates.
   *
   * @param pos the position of the square.
   * @return LIGHT color if the sum of row and column is even, DARK otherwise.
   */
  private def squareColor(pos: Position): ColorType =
    if ((pos.row + pos.col) % 2 == 0) ColorType.LIGHT else ColorType.DARK

  /**
   * Determines the initial piece setup for a specific square during board initialization.
   * Pieces are placed only on DARK squares in the first and last rows specified by Board configuration.
   *
   * @param position the position to initialize.
   * @return an Option containing a Man piece of the appropriate color, or None if the square should be empty.
   */
  private def initSquare(position: Position) = (position.row, position.col) match {
    case (r, _) if r < Board.PieceRowNumber && squareColor(position) == ColorType.DARK => Some(Man(ColorType.DARK))
    case (r, _) if r >= Board.Size - Board.PieceRowNumber && squareColor(position) == ColorType.DARK => Some(Man(ColorType.LIGHT))
    case _ => None
  }

  /**
   * The internal representation of the board as a 2D grid.
   * It is initialized as a Vector of Vectors (Board.SizexBoard.Size) where each square is 
   * instantiated with its specific color, position, and initial piece setup.
   */
  private var squares: Vector[Vector[Square]] =
    Vector.tabulate(Board.Size, Board.Size) { (row, col) =>

      val piece: Option[Piece] = initSquare(Position(row, col))

      BoardSquare(
        colorType = squareColor(Position(row, col)),
        position = Position(row, col),
        piece = piece
      )
    }


  /**
   * Retrieves the piece at a specific position.
   *
   * @param pos the board position to check.
   * @return an Option containing the Piece if present, None otherwise.
   */
  override def getPiece(pos: Position): Option[Piece] = squares(pos.row)(pos.col).piece

    /**
     * Retrieves the square object at a specific position.
     *
     * @param pos the board position to check.
     * @return an Option containing the Square if within bounds, None otherwise.
     */
    override def getSquare(pos: Position): Option[Square] =
      if (isInsideBoard(pos)) Some(squares(pos.row)(pos.col))
      else None

    /**
     * Returns the square at a given position.
     *
     * @param pos the board's position.
     * @return an optional square if the position is inside the board.
     */
    override def squareAt(pos: Position): Option[Square] = ???

    /**
     * Calculates all legal moves (both normal moves and captures) for a given square.
     *
     * @param from the starting square.
     * @return a list of possible destination squares.
     */
    override def possibleMoves(from: Square): List[Square] = ???

    /**
     * Executes a piece movement on the board.
     *
     * @param move the move object containing source, destination, and potential captured pieces.
     * @return true if the move was valid and successfully applied, false otherwise.
     */
    override def movePiece(move: Move): Boolean = ???

    /**
     * Returns an iterator over all squares on the board.
     *
     * @return an iterator of Square objects.
     */
    override def iterator: Iterator[Square] = ???

    /**
     * Returns a flat sequence of all squares.
     *
     * @return a Seq containing all Square objects.
     */
    override def allSquares: Seq[Square] = ???

    /**
     * Returns an iterator that provides pairs of positions and their corresponding squares.
     *
     * @return an iterator of (Position, Square) tuples.
     */
    override def iteratorWithPositions: Iterator[(Position, Square)] = ???

    /**
     * Provides a 2D matrix view of the board's squares.
     *
     * @return a Vector of Vectors representing the board grid.
     */
    override def squaresView: Vector[Vector[Square]] = ???

    /**
     * Identifies the square containing a captured piece between the start and end of a move.
     *
     * @param move the move performed.
     * @return an Option containing the captured Square, if one exists.
     */
    override def getCapturablePieceBetween(move: Move): Option[Square] = ???

    /**
     * Overloaded version to find a capturable piece between two squares for a specific piece.
     *
     * @param from        the starting square.
     * @param to          the destination square.
     * @param movingPiece the piece performing the move.
     * @return an Option containing the square with the piece to be captured.
     */
    override def getCapturablePieceBetween(from: Square, to: Square, movingPiece: Piece): Option[Square] = ???



/**
 * Companion object for Board.
 */
object Board {
  val Size: Int = 8
  val PieceRowNumber: Int = 3
}