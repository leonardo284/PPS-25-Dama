package model

import model.enums.ColorType

/**
 * Base trait for representing a player in the game.
 * @param name the player's name
 * @param color the color assigned to the player
 */
sealed trait Player:
  def name: String
  def color: ColorType
 
 
case class HumanPlayer(override val name: String, override val color: ColorType) extends Player
 
 /**
 * An AI-controlled player.
 **/
case class AIPlayer(override val color: ColorType,override val name: String = "AI") extends Player
  
