package model

import model.Position
import model.enums.ColorType
import scala.util.Random

trait Game {
  def currentBoard: Board

  def currentTurn: Player

  def makeMove(move: Move): Either[String, Move]

  def undoMove(): Boolean

  def getPlayers: (Player, Player)

  def getMoves: List[Move]
}