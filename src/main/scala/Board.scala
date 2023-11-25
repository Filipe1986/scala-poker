import scala.collection.mutable

object Board {

  object HandValue extends Enumeration {
    val HIGH_CARD, PAIR, TWO_PAIRS, THREE_OF_A_KIND, STRAIGHT, FLUSH, FULL_HOUSE, FOUR_OF_A_KIND, STRAIGHT_FLUSH = Value
  }
  def generateBoard(boardString: String): Seq[Card] = {
    boardString.grouped(2).map(s => Card(s)).toSeq
  }

  def StringToCards(cards: String): Seq[Card] = {
    cards.grouped(2).map(Card).toList
  }
}