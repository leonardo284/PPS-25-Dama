package model

import enums.{ColorType, GameType}
import enums.ColorType.{DARK, LIGHT}

import scala.util.Random

trait Game(val selectedMode: GameType):

  /**
   * Returns the current board.
   *
   * @return the board
   */
  def currentBoard: Board

  /**
   * Returns the player whose turn it is.
   *
   * @return the current player
   */
  def currentTurn: Player

  /**
   * Attempts to make a move.
   *
   * @param move the move to apply
   * @return either an error message or the applied move
   */
  def makeMove(move: Move): Either[String, Move]

  /**
   * Undoes the last move, if any.
   *
   * @return true if a move was undone, false otherwise
   */
  def undoMove(): Boolean

  /**
   * Retrieves the two players participating in the current game session.
   *
   * @return a tuple containing the first player (Light) and the second player (Dark).
   */
  def getPlayers: (Player, Player)

  /**
   * Provides the full history of moves performed during the match.
   * Useful for tracking undoing moves, or generating game logs.
   *
   * @return a list of Move objects representing the chronological sequence of plays.
   */
  def getMoves: List[Move]

  /**
   * Executes the movement logic for the AI-controlled player.
   */
  def makeAIMove() : Unit

  /**
   * Indicates if is AI Turn
   * @return true if is AI turn, else otherwise.
   */
  def isAITurn : Boolean

  /**
   * Determines if the game has ended.
   * A game is finished if the current player has no pieces left or no legal moves available.
   */
  def isGameFinished: Boolean

  /**
   * Returns the winner of the match, if any.
   * The winner is the player whose opponent has no pieces or no valid moves remaining.
   * @return an Option containing the winning Player, or None if the game is still ongoing.
   */
   def getWinner: Option[Player]



/**
 * Represents a single match of checkers.
 *
 * @param player1 first player
 * @param player2 second player
 */
class GameImpl(val player1: Player, val player2: Player, selectedMode: GameType) extends Game(selectedMode):
  val board: Board = CheckersBoard()

  private var movesHistory: List[Move] = List.empty

  private var turn: Player = player1.color match
    case ColorType.LIGHT => player1
    case _ => player2

  private def isMoveByCurrentPlayer(move: Move): Boolean = move.player == currentTurn &&
    move.from.piece.isDefined &&
    move.from.piece.get.color == currentTurn.color

  private def changeTurn(): Unit = turn = if (turn == player1) player2 else player1

  /**
   * Retrieves the two players participating in the current game session.
   *
   * @return a tuple containing the first player and the second player.
   */
  override def getPlayers: (Player, Player) = (player1, player2)


  /**
   * Provides the full history of moves performed during the match.
   * Useful for tracking undoing moves, or generating game logs.
   *
   * @return a list of Move objects representing the chronological sequence of plays.
   */
  override def getMoves: List[Move] = movesHistory


  /**
   * Returns the current board.
   *
   * @return the board
   */
  override def currentBoard: Board = board


  /**
   * Returns the player whose turn it is.
   *
   * @return the current player
   */
  override def currentTurn: Player = turn

  /**
   * Attempts to make a move.
   *
   * @param move the move to apply
   * @return either an error message or the applied move
   */
  override def makeMove(move: Move): Either[String, Move] =
    if (!isMoveByCurrentPlayer(move)) return Left("Invalid move")

    val promotionDetected = move.from.piece match {
      case Some(Man(ColorType.LIGHT)) => move.to.position.row == 0
      case Some(Man(ColorType.DARK)) => move.to.position.row == Board.Size - 1
      case _ => false
    }

    val finalMove = move match {
      case m: MoveImpl => m.copy(isPromotion = promotionDetected)
      case _ => move // Fallback if it's another implementation
    }

    val success = board.movePiece(finalMove)

    if (success) {
      movesHistory = finalMove :: movesHistory
      changeTurn()
      Right(finalMove)
    } else {
      Left("Invalid move")
    }

  /**
   * Undoes the last move, if any.
   *
   * @return true if a move was undone, false otherwise
   */
  override def undoMove(): Boolean = movesHistory match {
    case last :: rest =>
      board.undoMovePiece(last)
      movesHistory = rest
      turn = if (turn == player1) player2 else player1
      true
    case Nil => false
  }

  /**
   * Checks whether the current game mode is Player vs AI.
   * @return true if the selected game mode involves an AI opponent, false otherwise.
   */
  private def isAIGame: Boolean = selectedMode == GameType.PvAI

  /**
   * Identifies and retrieves the AI player participant, if one exists in the current game.
   * @return an Option containing the AIPlayer if found, or None if both players are human.
   */
  private def getAIPlayer: Option[Player] =
    (player1, player2) match
      case (ai: AIPlayer, _) => Some(ai)
      case (_, ai: AIPlayer) => Some(ai)
      case _ => None

  /**
   * Indicates if is AI Turn
   *
   * @return true if is AI turn, else otherwise.
   */
  override def isAITurn : Boolean = isAIGame && getAIPlayer.isDefined && getAIPlayer.get == turn

  /**
   * Executes the movement logic for the AI-controlled player.
   * In the event that multiple moves are available, this method ensures
   * compliance with mandatory capture rules.
   */
  override def makeAIMove(): Unit =
    if(isAIGame)
      val possibleMoves = board.getAllPossibleMoves(getAIPlayer.get)
      if(possibleMoves.nonEmpty)
        makeMove(possibleMoves.head)


  private def player1CanMove = board.getAllPossibleMoves(player1).nonEmpty
  private def player2CanMove = board.getAllPossibleMoves(player2).nonEmpty

  /**
   * Determines if the game has ended.
   * A game is finished if the current player has no pieces left or no legal moves available.
   */
  override def isGameFinished: Boolean =
    // The game is over if at least one player cannot move anymore
    !player1CanMove || !player2CanMove

  /**
   * Returns the winner of the match, if any.
   * The winner is the player whose opponent has no pieces or no valid moves remaining.
   * @return an Option containing the winning Player, or None if the game is still ongoing.
   */
  override def getWinner: Option[Player] =
    (player1CanMove, player2CanMove) match
      case (true, false) => Some(player1) // Player 2 is stuck, Player 1 wins
      case (false, true) => Some(player2) // Player 1 is stuck, Player 2 wins
      case _ => None                      // Either game is ongoing or it's an edge-case draw
/**
 * Companion object for Game.
 */
object Game:
  /** PvP Mode*/
  def apply(name1: String, name2: String): Game =
    val (color1, color2) = randomColors()
    val p1 = HumanPlayer(name1, color1)
    val p2 = HumanPlayer(name2, color2)
    new GameImpl(p1, p2, GameType.PvP)

  /** Player vs AI Mode*/
  def apply(playerName: String): Game =
    val (color1, color2) = randomColors()
    val p1 = HumanPlayer(playerName, color1)
    val p2 = AIPlayer(color2)
    new GameImpl(p1, p2, GameType.PvAI)

  private def randomColors(): (ColorType, ColorType) =
    if Random.nextBoolean() then (LIGHT, DARK) else (DARK, LIGHT)



