package model

import model.enums.ColorType


/**
 * Represents a single square on the board.
 */
sealed trait Square:

  def colorType: ColorType

  def position: Position

  def piece: Option[Piece]

  def isEmpty: Boolean = piece.isEmpty

  def containsMan: Boolean = piece.exists {
    case _: Man => true
    case _ => false
  }

  def containsKing: Boolean = piece.exists {
    case _: Man => false
    case _ => true
  }

/**
 * Class that implement the Square trait.
 * @param colorType the square's color
 * @param position the square's position
 * @param piece optional piece occupying the square
 */
case class BoardSquare(
  colorType: ColorType,
  position: Position,
  piece: Option[Piece] = None
) extends Square