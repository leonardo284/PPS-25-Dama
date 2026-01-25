package model.enums

enum GameType:
  case PvP, PvAI

  override def toString: String = this match
  case PvP => "Player vs Player"
  case PvAI => "Player vs AI"