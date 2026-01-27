package view

import controller.GameController
import model.Board
import model.Position

/**
 * Interface representing the view that displays the game state.
 * It provides methods to interact with the UI components and update the board display.
 */
trait GamePage:

  /**
   * Assigns the controller to the view to handle user input and coordinate with the model.
   * @param c the GameController instance.
   */
  def setController(c: GameController): Unit

  /**
   * Displays the game page to the user.
   */
  def show(): Unit

  /**
   * Updates the visual state of the board by rendering pieces in their current positions.
   * @param board the board state to be drawn.
   */
  def render(board: Board): Unit

  /**
   * Visually highlights a specific set of squares (e.g., to show available moves).
   * @param positions the list of coordinates to be highlighted.
   */
  def highlightSquares(positions: List[Position]): Unit

  /**
   * Displays an error message to the user (e.g., an invalid move notification).
   * @param errorMsg the description of the error.
   */
  def logError(errorMsg: String): Unit

  /**
   * Initializes the graphical components of the checkerboard (buttons, grid, etc.).
   */
  def initializeChecker(): Unit

  /**
   * Restores the default background colors of the board squares, removing any highlights.
   */
  def resetBoardColors(): Unit

