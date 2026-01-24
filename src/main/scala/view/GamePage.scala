package view

import controller.GameController
import model.Board
import model.Position

trait GamePage :
  def setController(c: GameController): Unit
  def show(): Unit
  def render(board: Board): Unit
  def highlightSquares(positions: List[Position]): Unit
  def logError(errorMsg: String): Unit
  def initializeChecker() : Unit
  def resetBoardColors(): Unit

