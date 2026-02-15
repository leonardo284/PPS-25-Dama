package view

import controller.GameController
import java.awt.{BorderLayout, Color, Dimension, FlowLayout, Font, GridLayout, Image}
import javax.swing.{BorderFactory, BoxLayout, ImageIcon, JButton, JFrame, JLabel, JOptionPane, JPanel, SwingConstants}
import model.{Board, Position}
import model.enums.ColorType
import model.enums.ColorType.{DARK}

class CheckersPage() extends GamePage:

  private val frame = new JFrame("Dama")
  private val boardPanel = new JPanel(new GridLayout(Board.Size, Board.Size))

  private val btnNewGame = new JButton("Nuova Partita")
  private val btnExit = new JButton("Esci")
  private val btnUndo = new JButton("Undo")

  private val p1Label = new JLabel("", SwingConstants.CENTER)
  private val p2Label = new JLabel("", SwingConstants.CENTER)
  private val errorLogLabel = new JLabel(" ", SwingConstants.CENTER)

  private var controller: GameController = _
  private val buttons = Array.ofDim[JButton](Board.Size, Board.Size)
  private val style = BoardStyles.Classic

  private val cellSize = 80

  private val blackPieceIcon = scaledIcon("black_piece.png", cellSize - 10)
  private val whitePieceIcon = scaledIcon("white_piece.png", cellSize - 10)
  private val blackKingIcon = scaledIcon("black_king.png", cellSize - 10)
  private val whiteKingIcon = scaledIcon("white_king.png", cellSize - 10)

  setupLayout()

  /**
   * Loads and resizes an image from the project's resource folder.
   * @param path the filename or relative path of the icon in the resources folder.
   * @param size the desired width and height for the square icon.
   * @return a scaled ImageIcon, or an empty ImageIcon if the resource is missing.
   */
  private def scaledIcon(path: String, size: Int): ImageIcon = {
    val resource = getClass.getResource(s"/$path")
    if (resource != null) {
      val img = new ImageIcon(resource).getImage
      val scaled = img.getScaledInstance(size, size, Image.SCALE_SMOOTH)
      new ImageIcon(scaled)
    } else {
      new ImageIcon()
    }
  }

  /**
   * Configures the graphical layout of the game page.
   * Organizes the player information, the board, and the message log using a BorderLayout.
   */
  private def setupLayout(): Unit = {
    frame.setLayout(new BorderLayout())

    // --- Top Panel Configuration ---
    val topContainer = new JPanel()
    topContainer.setLayout(new BoxLayout(topContainer, BoxLayout.Y_AXIS))

    // Buttons bar (Nuova Partita / Undo / Esci)
    val controlBar = new JPanel(new FlowLayout(FlowLayout.RIGHT))
    btnNewGame.setFocusable(false)
    btnUndo.setFocusable(false)
    btnExit.setFocusable(false)

    controlBar.add(btnNewGame)
    controlBar.add(btnUndo)
    controlBar.add(btnExit)

    // Players info
    val infoPanel = new JPanel(new GridLayout(1, 2))
    List(p1Label, p2Label).foreach { l =>
      l.setFont(new Font("SansSerif", Font.BOLD, 18))
      l.setBorder(BorderFactory.createEmptyBorder(5, 10, 5, 10))
    }
    infoPanel.add(p1Label)
    infoPanel.add(p2Label)

    topContainer.add(controlBar)
    topContainer.add(infoPanel)

    // --- Action Listeners ---
    btnNewGame.addActionListener(_ => {
      val res = JOptionPane.showConfirmDialog(frame, "Vuoi ricominciare la partita?", "Nuova Partita", JOptionPane.YES_NO_OPTION)
      if (res == JOptionPane.YES_OPTION) {
        frame.dispose()
        new MenuPage().show() // Back to Menu
      }
    })

    // Undo listener
    btnUndo.addActionListener(_ => {
      errorLogLabel.setText(" ")
      controller.undoMove()
    })

    btnExit.addActionListener(_ => {
      val res = JOptionPane.showConfirmDialog(frame, "Sei sicuro di voler uscire e tornare al menu?", "Conferma Uscita", JOptionPane.YES_NO_OPTION)
      if (res == JOptionPane.YES_OPTION) {
        frame.dispose()
        new MenuPage().show() // Back to Menu
      }
    })

    // --- Board Panel Configuration ---
    boardPanel.setBorder(BorderFactory.createLineBorder(new Color(60, 30, 10), 4))
    val boardDim = cellSize * Board.Size
    boardPanel.setPreferredSize(new Dimension(boardDim, boardDim))

    // --- Bottom Panel Configuration ---
    val bottomPanel = new JPanel(new FlowLayout())
    errorLogLabel.setFont(new Font("SansSerif", Font.PLAIN, 14))
    errorLogLabel.setForeground(Color.RED)
    bottomPanel.add(errorLogLabel)

    frame.add(topContainer, BorderLayout.NORTH)
    frame.add(boardPanel, BorderLayout.CENTER)
    frame.add(bottomPanel, BorderLayout.SOUTH)

    frame.pack()
    frame.setResizable(false)
    frame.setLocationRelativeTo(null)
    frame.setDefaultCloseOperation(javax.swing.WindowConstants.EXIT_ON_CLOSE)
  }

  /**
   * Updates the visual style of a player's label to indicate whether it is their turn.
   *
   * @param label    the JLabel to be updated.
   * @param isActive true if the label should be highlighted, false to reset it.
   */
  private def updateLabelHighlight(label: JLabel, isActive: Boolean): Unit = {
    if (isActive) {
      // Highlight the label with a background color
      label.setOpaque(true)
      label.setBackground(new Color(255, 235, 150))
    } else {
      // Reset the label to its transparent state
      label.setOpaque(false)
      label.setBackground(null)
    }
  }

  /**
   * Assigns the controller to the view to handle user input and coordinate with the model.
   *
   * @param c the GameController instance.
   */
  override def setController(c: GameController): Unit = {
    controller = c
    val (p1, p2) = controller.players

    p1Label.setText(p1.name)
    p1Label.setIcon(if (p1.color == DARK) blackPieceIcon else whitePieceIcon)

    p2Label.setText(p2.name)
    p2Label.setIcon(if (p2.color == DARK) blackPieceIcon else whitePieceIcon)
  }

  /**
   * Disables user interactions with the board and undo button (e.g., during AI turn).
   */
  override def disableInput(): Unit = {
    frame.getContentPane.setCursor(java.awt.Cursor.getPredefinedCursor(java.awt.Cursor.WAIT_CURSOR))
    btnUndo.setEnabled(false) // Always disable undo during AI turn to prevent conflicts

    // Disable all clickable buttons
    for {
      row <- 0 until Board.Size
      col <- 0 until Board.Size
      btn = buttons(row)(col)
      if (row + col) % 2 != 0 // Only dark squares (the clickable ones)
    } {
      btn.setEnabled(false)
      btn.setDisabledIcon(btn.getIcon)
    }
  }

  /**
   * Re-enables user interactions with the board and undo button.
   */
  override def enableInput(): Unit = {
    frame.getContentPane.setCursor(java.awt.Cursor.getDefaultCursor)
    btnUndo.setEnabled(controller.canUndo) // Only enable undo if the controller says we actually can

    // Re-enable buttons on dark squares
    for {
      row <- 0 until Board.Size
      col <- 0 until Board.Size
      btn = buttons(row)(col)
      if (row + col) % 2 != 0
    } btn.setEnabled(true)
  }


  /**
   * Initializes the graphical components of the checkerboard.
   */
  override def initializeChecker(): Unit = {
    boardPanel.removeAll()
    for ((pos, square) <- controller.iteratorWithPositions) {
      val button = new JButton()
      buttons(pos.row)(pos.col) = button

      button.setFocusPainted(false)
      button.setFocusable(false)
      button.setRolloverEnabled(false)
      button.setMargin(new java.awt.Insets(0, 0, 0, 0))

      if (square.colorType == ColorType.LIGHT) {
        button.setBackground(style.light)
        button.setEnabled(false)
      } else {
        button.setBackground(style.dark)
        button.addActionListener(_ => {
          errorLogLabel.setText(" ")
          controller.onSquareClicked(pos)
        })
      }
      boardPanel.add(button)
    }
  }

  /**
   * Displays the game page to the user.
   */
  override def show(): Unit = frame.setVisible(true)

  /**
   * Restores the default background colors of the board squares, removing any highlights.
   */
  override def resetBoardColors(): Unit = {
    for (row <- 0 until Board.Size; col <- 0 until Board.Size) {
      val btn = buttons(row)(col)
      if ((row + col) % 2 == 0) btn.setBackground(style.light)
      else btn.setBackground(style.dark)
      if (btn.getIcon == null) btn.setDisabledIcon(null)
    }
  }

  /**
   * Visually highlights a specific set of squares (e.g., to show available moves).
   *
   * @param positions the list of coordinates to be highlighted.
   */
  override def highlightSquares(positions: List[Position]): Unit = {
    resetBoardColors()
    positions.foreach { pos =>
      val btn = buttons(pos.row)(pos.col)
      btn.setBackground(new Color(150, 255, 150))
      if (btn.getIcon == null) btn.setDisabledIcon(null)
    }
  }

  /**
   * Updates the visual state of the board by rendering pieces in their current positions.
   *
   * @param board the board state to be drawn.
   */
  override def render(board: model.Board): Unit = {
    board.squaresView.foreach(_.foreach { square =>
      val button = buttons(square.position.row)(square.position.col)
      square.piece match {
        case Some(man: model.Man) =>
          button.setIcon(if (man.color == DARK) blackPieceIcon else whitePieceIcon)
        case Some(king: model.King) =>
          button.setIcon(if (king.color == DARK) blackKingIcon else whiteKingIcon)
        case None =>
          button.setIcon(null)
          button.setDisabledIcon(null)
      }
    })

    btnUndo.setEnabled(controller.canUndo)

    val activePlayer = controller.currentPlayer
    updateLabelHighlight(p1Label, p1Label.getText == activePlayer.name)
    updateLabelHighlight(p2Label, p2Label.getText == activePlayer.name)
    boardPanel.repaint()
  }


  /**
   * Displays an error message to the user (e.g., an invalid move notification).
   *
   * @param errorMsg the description of the error.
   */
  override def logError(errorMsg: String): Unit = errorLogLabel.setText(errorMsg)

  /**
   * Displays a message to announce the winner.
   *
   * @param winnerName the name of the player who won the match.
   */
  override def showWinner(winnerName: String): Unit =
    // Ensure the UI reflects the final state before showing the popup
    render(controller.game.currentBoard)

    // Disable inputs so players cannot move or undo after the game is over
    disableInput()
    btnUndo.setEnabled(false)
    btnNewGame.setEnabled(true)

    // Create a custom message or use a standard popup
    val message = s" $winnerName ha vinto!!!"
    val title = "Partita terminata!"

    // Show the dialog on the Event Dispatch Thread
    javax.swing.SwingUtilities.invokeLater(() => {
      JOptionPane.showMessageDialog(
        frame,
        message,
        title,
        JOptionPane.INFORMATION_MESSAGE,
        if (winnerName == p1Label.getText) p1Label.getIcon else p2Label.getIcon
      )
      // go to menu page
      frame.dispose()
      MenuPage().show()
    })
