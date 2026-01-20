package model

import model.enums.{ColorType, PieceType}

/**
 * Represents a generic piece in the game.
 * A piece always has a color.
 */
sealed trait Piece:
  def color: ColorType
  def pieceType: PieceType

/**
 * Represents a regular (non-king) checkers piece.
 * @param color the color of the piece
 */
case class Man(color: ColorType) extends Piece:
  override val pieceType: PieceType = PieceType.Man

/**
 * Represents a king piece in checkers.
 * @param color the color of the piece
 */
case class King(color: ColorType) extends Piece:
  override val pieceType: PieceType = PieceType.King