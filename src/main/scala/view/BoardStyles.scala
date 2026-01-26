package view

import java.awt.Color

case class BoardStyle(light: Color, dark: Color)

object BoardStyles {
  val Classic = BoardStyle(Color.WHITE, Color.DARK_GRAY)
//  val Gray = BoardStyle(new Color(220, 220, 220), new Color(100, 100, 100))
//  val Brown = BoardStyle(new Color(245, 222, 179), new Color(139, 69, 19))
//  val Green = BoardStyle(new Color(200, 230, 200), new Color(34, 85, 34))
//  val Blue = BoardStyle(new Color(200, 220, 240), new Color(30, 60, 120))
}