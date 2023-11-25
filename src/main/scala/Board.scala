import scala.collection.mutable

object Board {
  def generateBoard(boardString: String): Seq[Card] = {
    boardString.grouped(2).map(s => Card(s)).toSeq
  }

  def StringToCards(cards: String): Seq[Card] = {
    cards.grouped(2).map(Card).toList
  }
}