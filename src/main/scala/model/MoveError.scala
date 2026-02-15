package model

sealed trait MoveError:
  def message: String

case object NotYourPiece extends MoveError:
  override val message: String = "Il pezzo selezionato non è tuo o la casella è vuota!"

case object IllegalMove extends MoveError:
  override val message: String = "Mossa non valida!"

case object NoMovesToUndo extends MoveError:
  override val message: String = "Nessuna mossa da annullare!"

case object WaitingForAI extends MoveError:
  override val message: String = "Attendi il turno dell'IA prima di annullare!"