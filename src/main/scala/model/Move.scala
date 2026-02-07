package model


/**
 * Represents a move from one position to another.
 */
trait Move:
  def from: Square
  def to: Square
  def player: Player
  def captured: Option[Square]
  def isPromotion: Boolean

  /**
   * Checks if the move involves capturing an opponent's piece.
   *
   * @return true if there is a captured square, false otherwise.
   */
  def isCapture: Boolean = captured.isDefined


/**
 * The class that implement the Move trait.
 * @param from starting position
 * @param to destination position
 * @param player player that moves
 * @param captured optional captured piece
 */
case class MoveImpl(
  from: Square,
  to: Square,
  captured: Option[Square] = None,
  player: Player,
  isPromotion: Boolean
) extends Move




