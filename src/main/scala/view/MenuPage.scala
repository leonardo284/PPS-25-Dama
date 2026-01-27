package view

import java.awt.FlowLayout
import javax.swing._
import controller.CheckersController
import model.GameImpl
import model.enums.GameType

/**
 * Represents the main menu of the application.
 * Allows users to configure player names and select the game mode.
 */
class MenuPage {

  private val frame = new JFrame("Configurazione Giocatori")

  private val player1Field = new JTextField("Player1", 15)
  private val player2Field = new JTextField("Player2", 15)

  private val modeComboBox = new JComboBox[GameType](GameType.values)

  private val startButton = new JButton("Start")
  private val exitButton = new JButton("Exit")

  private val mainPanel = new JPanel()
  mainPanel.setLayout(new BoxLayout(mainPanel, BoxLayout.Y_AXIS))
  mainPanel.setBorder(BorderFactory.createEmptyBorder(20, 20, 20, 20))

  private def labeledPanel(label: String, component: JComponent): JPanel = {
    val p = new JPanel(new FlowLayout(FlowLayout.LEFT))
    p.add(new JLabel(label))
    p.add(component)
    p
  }

  mainPanel.add(labeledPanel("Modalità:", modeComboBox))
  mainPanel.add(Box.createVerticalStrut(10))
  mainPanel.add(labeledPanel("Giocatore 1:", player1Field))
  mainPanel.add(Box.createVerticalStrut(10))
  mainPanel.add(labeledPanel("Giocatore 2:", player2Field))
  mainPanel.add(Box.createVerticalStrut(20))

  private val buttonPanel = new JPanel(new FlowLayout(FlowLayout.CENTER, 20, 0))
  buttonPanel.add(startButton)
  buttonPanel.add(exitButton)
  mainPanel.add(buttonPanel)

  // --- Event Listeners ---
  modeComboBox.addActionListener(_ => {
    val selectedMode = modeComboBox.getSelectedItem.asInstanceOf[GameType]

    val isVsAI = selectedMode == GameType.PvAI

    player2Field.setEnabled(!isVsAI)
    if (isVsAI) {
      player2Field.setText("AI")
    } else if (player2Field.getText == "AI") {
      player2Field.setText("Player2")
    }
  })

  // Start game logic
  startButton.addActionListener(_ => {
    val p1Name = player1Field.getText.trim
    val p2Name = player2Field.getText.trim
    val selectedMode = modeComboBox.getSelectedItem.asInstanceOf[GameType]

    if (p1Name.isEmpty || p2Name.isEmpty) {
      JOptionPane.showMessageDialog(frame, "I nomi non possono essere vuoti", "Errore", JOptionPane.ERROR_MESSAGE)
    } else {
      frame.dispose()

      // Initialize the Game on the Event Dispatch Thread
      SwingUtilities.invokeLater { () =>
        val model = new GameImpl(p1Name, p2Name, selectedMode)
        val view = new CheckersPage()
        val controller = new CheckersController(model, view)

        controller.startView()
      }
    }
  })

  // Exit application
  exitButton.addActionListener(_ => {
    frame.dispose()
    System.exit(0)
  })

  frame.add(mainPanel)
  frame.setSize(400, 300)
  frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)

  /**
   * Displays the menu window centered on the screen.
   */
  def show(): Unit = {
    frame.setLocationRelativeTo(null)
    frame.setVisible(true)
  }
}