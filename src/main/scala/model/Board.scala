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
  private def hasCapturablePieceBetween(move: Move): Boolean = getCapturablePieceBetween(move).isDefined

  /**
   * Checks if there is a capturable regular piece (Man) between the starting and destination positions
   */
  private def hasCapturableManPieceBetween(move: Move): Boolean = getCapturablePieceBetween(move).exists(_.containsMan)
  
  /**
   * Checks if the starting square is within the board boundaries and is a dark square
   */
  private def isValidMoveStartingPosition(square: Square): Boolean =  isInsideBoard(square.position) && square.colorType == ColorType.DARK

  private def isValidManMove(move: Move, piece: Man): Boolean =
    val rowDiff = move.to.position.row - move.from.position.row
    val colDiff = move.to.position.col - move.from.position.col
    val rowDir = if (piece.color == DARK) 1 else -1

    val isNormal = rowDiff == rowDir && math.abs(colDiff) == 1
    val isCapture = rowDiff == 2 * rowDir && math.abs(colDiff) == 2 && hasCapturableManPieceBetween(move)

    isNormal || isCapture

  private def isValidKingMove(move: Move, piece: King): Boolean =
    val absRow = math.abs(move.to.position.row - move.from.position.row)
    val absCol = math.abs(move.to.position.col - move.from.position.col)

    val isNormal = absRow == 1 && absCol == 1
    val isCapture = absRow == 2 && absCol == 2 && hasCapturablePieceBetween(move)

    isNormal || isCapture

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
    val isStructureValid =
      isInsideBoard(move.to.position) &&
        move.to.colorType == DARK &&
        move.to.piece.isEmpty

    if !isStructureValid then false
    else
      move.from.piece.fold(false) {
        case m: Man  => isValidManMove(move, m)
        case k: King => isValidKingMove(move, k)
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
    move.from.piece.isDefined &&                // Ensure there is actually a piece at the starting position
    isValidMoveStartingPosition(move.to) &&     // The destination square must be a valid board square
    isValidMoveDestinationPosition(move) &&     // Verify if the specific destination is legal for this type of move
    isCaptureAllowed(move)                      // Check if a capture (jump) is required or allowed by the rules

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
    from.piece.fold(List.empty) { piece =>

      // Iterate through all potential directions based on piece type (2 for Man, 4 for King).
      // Each direction is a tuple (dr = direction row, dc = direction column).
      // Using flatMap to concatenate the valid destination squares from all directions.

      moveDirections(piece).flatMap { (dr, dc) =>
        val nextPos = Position(from.position.row + dr, from.position.col + dc)

        getSquare(nextPos) match
          // Simple Move (empty square)
          case Some(s) if s.piece.isEmpty => List(s)

          // Potential Capture (Jump)
          case Some(s) if s.piece.exists(_.color != piece.color) =>
            // A Man can't eat a king
            val canJump = (piece, s.piece) match
              case (Man(_), Some(King(_))) => false
              case _ => true

            if canJump then
              val jumpPos = Position(from.position.row + 2 * dr, from.position.col + 2 * dc)
              // The move is valid only if the landing square is within bounds and empty.
              getSquare(jumpPos).filter(_.piece.isEmpty).toList
            else
              Nil

          case _ => Nil  // Square is out of bounds or occupied by a friendly piece.
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
  

  private def updateInternalSquare(pos: Position, piece: Option[Piece]): Unit =
    squares = squares.updated(pos.row,
      squares(pos.row).updated(pos.col,
        BoardSquare(squareColor(pos), pos, piece)
      )
    )

  /**
   * Executes a piece movement on the board.
   *
   * @param move the move object containing source, destination, and potential captured pieces.
   * @return true if the move was valid and successfully applied, false otherwise.
   */
  override def movePiece(move: Move): Boolean =
    if !isValidMove(move) then false
    else
      val from = move.from
      val to = move.to

      // Check for King promotion
      val finalPiece = from.piece.map {
        case Man(LIGHT) if to.position.row == 0 => King(LIGHT)
        case Man(DARK) if to.position.row == Board.Size - 1 => King(DARK)
        case p => p
      }

      // Create a list with the square to reset (from + captured)
      val positionsToEmpty = from.position :: move.captured.map(_.position).toList
      positionsToEmpty.foreach(pos => updateInternalSquare(pos, None))

      // Update the destination square (place the moving piece or the moving King)
      updateInternalSquare(to.position, finalPiece)

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
    val midSquare = getSquare(midPos)

    midSquare match
      case Some(s) if s.piece.exists(_.color != movingPiece.color) && to.isEmpty => Some(s)
      case _ => None
    
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
      val kingJumps = jumpMoves.filter(_.from.piece.exists(_.isInstanceOf[King]))

      if kingJumps.nonEmpty then kingJumps
      else if jumpMoves.nonEmpty then jumpMoves
      else allPotentialMoves


/**
 * Companion object for Board.
 */
object Board:
  val Size: Int = 8
  val PieceRowNumber: Int = 3
