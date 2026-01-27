import view.MenuPage

import javax.swing.SwingUtilities

object Main extends App :
  SwingUtilities.invokeLater { () =>
    new MenuPage().show()
  }


