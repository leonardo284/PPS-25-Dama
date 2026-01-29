package model

import enums.{ColorType, GameType, PieceType}
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
 * Represents a single match of checkers.
 *
 * @param player1Name first player name
 * @param player2Name second player name
 */
class GameImpl(player1Name: String, player2Name: String, selectedMode: GameType) extends Game(selectedMode):
  val board: Board = CheckersBoard()

  private val (player1, player2) = assignColors(player1Name, player2Name)

  private var movesHistory: List[Move] = List.empty

  private var turn: Player = player1.color match
    case ColorType.LIGHT => player1
    case _ => player2

  private def assignColors(name1: String, name2: String): (Player, Player) =
    if Random.nextBoolean() then
      (Player(name1, ColorType.LIGHT), Player(name2, ColorType.DARK))
    else
      (Player(name1, ColorType.DARK), Player(name2, ColorType.LIGHT))

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
      var possibleMoves = board.getAllPossibleMoves(getAIPlayer.get)
      if(possibleMoves.nonEmpty)
        makeMove(possibleMoves.head)

