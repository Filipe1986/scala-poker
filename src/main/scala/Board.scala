import scala.collection.mutable

object Board {
  def generateBoard(boardString: String): mutable.ArrayBuffer[Card] = {
    val boardCards = boardString.grouped(2).toArray

    var board: mutable.ArrayBuffer[Card] = new mutable.ArrayBuffer()

    boardCards.foreach(c => {
      board.append(Card(c))
    })
    board
  }

  def StringToCards(cardString: String): mutable.ArrayBuffer[Card] = {

    val arrayOfSubstrings = cardString.grouped(2).toArray
    var cards: mutable.ArrayBuffer[Card] = new mutable.ArrayBuffer()

    arrayOfSubstrings.foreach(s => {
      val card = Card(s)
      cards.append(card)
    })
    cards
  }
}