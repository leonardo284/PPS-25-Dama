package model

import model.enums.ColorType

/**
 * Represents a player in the game.
 * @param name the player's name
 * @param color the color assigned to the player
 */
case class Player(
   name: String,
   color: ColorType
 )
