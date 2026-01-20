package model.enums

enum PieceType(val displayName: String): 
  case Man extends PieceType("Man")
  case King extends PieceType("King")