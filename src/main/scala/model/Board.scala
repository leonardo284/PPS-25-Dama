package model

import enums.{ColorType, PieceType}
import enums.ColorType.{DARK, LIGHT}


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
  def allSquares: List[Square]

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
   * Reverts the state of the board based on the provided move.
   *
   * @param move The move to be undone.
   * @return true if the board was successfully restored.
   */
  def undoMovePiece(move: Move): Boolean


  /**
   * Calculates and returns all legal moves currently available for a specific player.
   * This method iterates through every piece on the board belonging to the 
   * specified color and aggregates all valid actions, including simple 
   * diagonal moves and jumps (captures).
   *
   * @param player The player for whom to calculate available moves.
   * @return A list of all valid Move objects for the selected player.
   */
  def getAllPossibleMoves(player: Player) : List[Move]

  /**
   * Calculates and returns all legal moves from a square for a specific player.
   *
   * @param player The player for whom to calculate available moves.
   * @param from The square from to move.
   * @return A list of all valid Move objects for the selected player.
   */
  def getAllPossibleMovesFromSquare(player: Player, from: Square): List[Move] = getAllPossibleMoves(player).filter(m => m.from == from)


  //def getPiecesWithColor(color: ColorType): List[Piece] = allSquares.filter(s => s.piece.isDefined && s.piece.get.color == color)
    //                                                                  .map(s => s.piece.get)

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
   * Checks if there is a piece that can be captured (jumped) between the starting and destination positions
   */
  private def hasCapturablePieceBetween(move: Move): Boolean =
    getCapturablePieceBetween(move).isDefined

  /**
   * Checks if there is a capturable regular piece (Man) between the starting and destination positions
   */
  private def hasCapturableManPieceBetween(move: Move): Boolean =
    getCapturablePieceBetween(move).exists(_.containsMan)
  
  /**
   * Checks if the starting square is within the board boundaries and is a dark square
   */
  private def isValidMoveStartingPosition(square: Square): Boolean = 
    isInsideBoard(square.position) && square.colorType == ColorType.DARK

  /**
   * Verifies if the destination square is valid for the specific piece making the move.
   * Validates basic board constraints (boundaries, color, occupancy) and then checks 
   * move-specific logic for both Men (regular pieces) and Kings, including normal 
   * steps and capture jumps.
   *
   * @param move the move to be validated
   * @return true if the destination follows the movement rules for the piece type
   */
  private def isValidMoveDestinationPosition(move: Move): Boolean =
    val from = move.from
    val to = move.to

    val isStructureValid =
      isInsideBoard(to.position) &&
        to.colorType == DARK &&
        to.piece.isEmpty

    if !isStructureValid then
      false
    else
      from.piece match {
        case None => false
        case Some(piece) =>
          val rowDiff = to.position.row - from.position.row
          val colDiff = to.position.col - from.position.col

          piece match {
            // ---------------- MAN ----------------
            case Man(color) =>
              val rowDir = if (color == ColorType.DARK) 1 else -1

              val normalMove =
                rowDiff == rowDir &&
                  math.abs(colDiff) == 1 /*&&
                      isValidDestinationPiece(move)*/

              val captureMove =
                rowDiff == 2 * rowDir &&
                  math.abs(colDiff) == 2 &&
                  hasCapturableManPieceBetween(move) // If a Man is being moved, it can only capture other Man
              normalMove || captureMove

            // ---------------- KING ----------------
            case King(_) =>
              val absRow = math.abs(rowDiff)
              val absCol = math.abs(colDiff)

              val normalMove =
                absRow == 1 &&
                  absCol == 1 /*&&
                      isValidDestinationPiece(move)*/

              val captureMove =
                absRow == 2 &&
                  absCol == 2 &&
                  hasCapturablePieceBetween(move)

              normalMove || captureMove
          }
      }

  /**
   * Verifies that a regular piece (Man) cannot capture a King 
   */
  private def isCaptureAllowed(move: Move): Boolean = (move.from.piece, move.to.piece) match
    case (Some(pieceFrom), Some(pieceTo)) => pieceFrom.pieceType == PieceType.King && pieceTo.pieceType == PieceType.Man ||
      pieceFrom.pieceType == pieceTo.pieceType
    case _ => true

  /**
   * Validates whether a move is legal according to the game rules.
   *
   * @param move the move to be validated
   * @return true if the move follows all game rules, false otherwise
   */
  private def isValidMove(move: Move): Boolean =

    // Ensure there is actually a piece at the starting position
    if (move.from.piece.isEmpty) return false

    // The destination square must be a valid board square
    if (!isValidMoveStartingPosition(move.to)) return false
    
    // Verify if the specific destination is legal for this type of move
    if (!isValidMoveDestinationPosition(move)) return false
    
    // Check if a capture (jump) is required or allowed by the rules
    if (!isCaptureAllowed(move)) return false

    true

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
   * Defines the possible movement directions for a piece based on its type and color.
   * Each tuple (rowDelta, colDelta) represents a diagonal step.
   */
  private def moveDirections(piece: Piece): List[(Int, Int)] = piece match {
    case Man(ColorType.DARK) => List((1, -1), (1, 1))
    case Man(ColorType.LIGHT) => List((-1, -1), (-1, 1))
    case King(_) =>
      List(
        (1, -1), (1, 1),
        (-1, -1), (-1, 1)
      )
  }

  /**
   * Calculates all legal reachable destinations from the given square.
   *
   * @param from the starting square.
   * @return a list of possible destination squares.
   */
  private def possibleDestinationsFromSquare(from: Square): List[Square] =
    from.piece match {
      case None => List.empty
      case Some(piece) =>
        // Iterate through all potential directions based on piece type (2 for Man, 4 for King).
        // Each direction is a tuple (dr = direction row, dc = direction column).
        // Using flatMap to concatenate the valid destination squares from all directions.
        moveDirections(piece).flatMap { case (dr, dc) =>
          val nextPos = Position(from.position.row + dr, from.position.col + dc)

          squareAt(nextPos) match {
            // Simple Move (empty square)
            case Some(square) if square.piece.isEmpty => List(square)

            // Potential Capture (Jump)
            case Some(square) if square.piece.exists(_.color != piece.color) =>

              val victim = square.piece.get

              // A Man can't eat a king
              val canJump = (piece, victim) match {
                case (Man(colorPiece), King(colorVictim)) => false
                case _ => true
              }

              if canJump then
                val jumpPos = Position(
                  from.position.row + (2 * dr),
                  from.position.col + (2 * dc)
                )

                // The move is valid only if the landing square is within bounds and empty.
                squareAt(jumpPos) match {
                  case Some(landing) if landing.piece.isEmpty => List(landing)
                  case _ => Nil
                }
              else
                Nil // If it's a Man trying to eat a King, return an empty list for this direction.

            // Square is out of bounds or occupied by a friendly piece.
            case _ => Nil
          }
        }
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
  override def squareAt(pos: Position): Option[Square] =
    if isInsideBoard(pos)
    then Some(squares(pos.row)(pos.col))
    else None

  /**
   * Executes a piece movement on the board.
   *
   * @param move the move object containing source, destination, and potential captured pieces.
   * @return true if the move was valid and successfully applied, false otherwise.
   */
  override def movePiece(move: Move): Boolean =
    if (!isValidMove(move)) return false
  
    val from = move.from
    val to = move.to
    val piece = from.piece.get
    val captured = move.captured
  
    // Update the starting square (remove the piece)
    val rowAfterFrom = squares(from.position.row)
      .updated(from.position.col, BoardSquare(from.colorType, from.position, None))
    squares = squares.updated(from.position.row, rowAfterFrom)
  
    // Empty the captured square (if any)
    captured.foreach { capturedSquare =>
      val rowAfterCapture = squares(capturedSquare.position.row)
        .updated(capturedSquare.position.col, BoardSquare(capturedSquare.colorType, capturedSquare.position, None))
      squares = squares.updated(capturedSquare.position.row, rowAfterCapture)
    }
  
    // Check for King promotion
    val promotedPiece = piece match {
      case Man(LIGHT) if to.position.row == 0 =>
        King(LIGHT)
      case Man(DARK) if to.position.row == Board.Size - 1 =>
        King(DARK)
      case _ => piece
    }
  
    // Update the destination square (place the piece or the King)
    val rowAfterTo = squares(to.position.row)
      .updated(to.position.col, BoardSquare(to.colorType, to.position, Some(promotedPiece)))
    squares = squares.updated(to.position.row, rowAfterTo)
  
    true
      
  /**
   * Returns an iterator over all squares on the board.
   *
   * @return an iterator of Square objects.
   */
  override def iterator: Iterator[Square] = squares.iterator.flatMap(_.iterator)

  /**
   * Returns a flat sequence of all squares.
   *
   * @return a Seq containing all Square objects.
   */
  override def allSquares: List[Square] = squares.flatten.toList

  /**
   * Returns an iterator that provides pairs of positions and their corresponding squares.
   *
   * @return an iterator of (Position, Square) tuples.
   */
  override def iteratorWithPositions: Iterator[(Position, Square)] =
    for {
      row <- squares.indices.iterator
      col <- squares(row).indices.iterator
    } yield (Position(row, col), squares(row)(col))

  /**
   * Provides a 2D matrix view of the board's squares.
   *
   * @return a Vector of Vectors representing the board grid.
   */
  override def squaresView: Vector[Vector[Square]] = squares

  /**
   * Identifies the square containing a captured piece between the start and end of a move.
   *
   * @param move the move performed.
   * @return an Option containing the captured Square, if one exists.
   */
  override def getCapturablePieceBetween(move: Move): Option[Square] =
    for
      piece <- move.from.piece
      square <- getCapturablePieceBetween(move.from, move.to, piece)
    yield square

  /**
   * Overloaded version to find a capturable piece between two squares for a specific piece.
   *
   * @param from        the starting square.
   * @param to          the destination square.
   * @param movingPiece the piece performing the move.
   * @return an Option containing the square with the piece to be captured.
   */
  override def getCapturablePieceBetween(from: Square, to: Square, movingPiece: Piece): Option[Square] =
    val midRow = (from.position.row + to.position.row) / 2
    val midCol = (from.position.col + to.position.col) / 2
    val midPos = Position(midRow, midCol)
    val midSquare = squareAt(midPos)

    if (midSquare.isDefined && midSquare.get.piece.exists(_.color != movingPiece.color) && to.isEmpty)
      midSquare
    else 
      None
    
  override def undoMovePiece(move: Move): Boolean =
    val from = move.from
    val to = move.to
    val pieceNowAtTo = getPiece(to.position)

    pieceNowAtTo match {
      case Some(piece) =>
        // 1. Restore the piece to its original position (from)
        // Check if it was a Man that got promoted to King during this move
        val pieceToRestore = if (move.isPromotion) Man(piece.color) else piece

        updateSquare(from.position, Some(pieceToRestore))

        // 2. Clear the destination square (to)
        updateSquare(to.position, None)

        // 3. Restore the captured piece, if any
        move.captured.foreach { capturedSquare =>
          // We assume the captured piece's info is stored within the Move object
          updateSquare(capturedSquare.position, capturedSquare.piece)
        }
        true

      case None => false // Should not happen if history is consistent
    }

  /** 
   * Helper to update a single square in the 2D Vector
   */
  private def updateSquare(pos: Position, piece: Option[Piece]): Unit =
    val row = squares(pos.row).updated(pos.col, BoardSquare(squareColor(pos), pos, piece))
    squares = squares.updated(pos.row, row)

  /**
   * Calculates and returns all legal moves currently available for a specific player.
   * This method iterates through every piece on the board belonging to the 
   * specified color and aggregates all valid actions, including simple 
   * diagonal moves and jumps (captures).
   *
   * @param player The player for whom to calculate available moves.
   * @return A list of all valid Move objects for the selected player.
   */
  override def getAllPossibleMoves(player: Player): List[Move] =

    val allPotentialMoves = allSquares
      .filter(s => s.piece.exists(_.color == player.color)) // Filtering current player's pieces
      .flatMap { fromSquare =>
        // for each piece get the possible moves
        possibleDestinationsFromSquare(fromSquare).map { toSquare =>
          // Create the Move object
          val capturedSquare = getCapturablePieceBetween(fromSquare, toSquare, fromSquare.piece.get)

          // Check if this move results in a promotion to King
          val willBePromoted = fromSquare.piece match {
            case Some(Man(LIGHT)) if toSquare.position.row == 0 => true
            case Some(Man(DARK)) if toSquare.position.row == Board.Size - 1 => true
            case _ => false
          }

          MoveImpl(
            from = fromSquare,
            to = toSquare,
            captured = capturedSquare,
            player = player,
            isPromotion = willBePromoted
          )
        }
      }

    // Apply the mandatory capture rule:
    // If jump moves exist, return only those.
    // Otherwise, return all simple moves.
    val jumpMoves = allPotentialMoves.filter(_.captured.nonEmpty)

    if jumpMoves.nonEmpty then jumpMoves else allPotentialMoves


/**
 * Companion object for Board.
 */
object Board:
  val Size: Int = 8
  val PieceRowNumber: Int = 3
