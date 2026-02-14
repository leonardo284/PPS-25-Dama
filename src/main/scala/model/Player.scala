package model

import model.enums.ColorType

/**
 * Base trait for representing a player in the game.
 */
sealed trait Player:
  def name: String
  def color: ColorType


/**
 * Represents a human participant in the game.
 * @param name  The display name chosen by the user.
 * @param color The color assigned to the player (LIGHT or DARK), which also determines the starting turn and direction.
 */
case class HumanPlayer(override val name: String, override val color: ColorType) extends Player

/**
 * Represents an AI-controlled opponent.
 * @param color The color assigned to the AI.
 * @param name  The name of the AI player, defaults to "AI".
 * This class identifies the player as an artificial entity, allowing the controller to trigger automated move calculations.
 */
case class AIPlayer(override val color: ColorType, override val name: String = "AI") extends Player