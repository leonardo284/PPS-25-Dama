import controller.{CheckersController, GameController}
import model.GameImpl
import model.enums.ColorType
import view.{CheckersPage, MenuPage}

import javax.swing.SwingUtilities

object Main extends App :
  SwingUtilities.invokeLater { () =>
    new MenuPage().show()
  }


