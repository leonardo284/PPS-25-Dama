package model

import enums.{ColorType, PieceType}
import enums.ColorType.{DARK, LIGHT}
import scala.util.Random

trait Game {
  def currentBoard: Board

  def currentTurn: Player

  def makeMove(move: Move): Either[String, Move]

  def undoMove(): Boolean

  def getPlayers: (Player, Player)

  def getMoves: List[Move]
}


/**
 * Represents a single match of checkers.
 *
 * @param player1Name first player name
 * @param player2Name second player name
 */
class GameImpl(player1Name: String, player2Name: String) extends Game:
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
  
  override def getPlayers: (Player, Player) = (player1, player2)

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
    if (!isMoveByCurrentPlayer(move))
      return Left("Invalid move")
  
    val success = board.movePiece(move)
  
    if (success) {
      movesHistory = move :: movesHistory
      changeTurn()
      Right(move)
    } else {
      Left("Invalid move")
    }

  override def undoMove(): Boolean = ???
  

