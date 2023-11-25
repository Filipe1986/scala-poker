import scala.collection.mutable

trait PokerGame {
  def sortHands(): String
}

class OmahaHold(line: String) extends PokerGame {
  var hands: mutable.ArrayBuffer[Hand] = new mutable.ArrayBuffer()
  override def sortHands(): String = {


    val boardCards = line.split(" ")(1).grouped(2).toArray
    val handsString = line.split(" ").tail

    var board: mutable.ArrayBuffer[Card] = new mutable.ArrayBuffer()

    handsString.foreach(hs => {
      hands.append(Hand(Board.StringToCards(hs), null))
    })

    hands.sortInPlaceBy(_.handClassification)

    hands.toString()
  }
}

class TexasHold(line: String) extends PokerGame {
  override def sortHands(): String = {

    val parts = line.split(" ")
    val board = Board.generateBoard(parts(1))

    val hands = parts.drop(2).map(hs => Hand(Board.StringToCards(hs), board))
    hands.foreach(h => h.getHandClassification())
    hands.sorted(Hand.handSorting).mkString(" ")
  }
}

class FiveCardDraw(line: String) extends PokerGame {
  var hands: mutable.ArrayBuffer[Hand] = new mutable.ArrayBuffer()
  override def sortHands(): String = {

    val handsString = line.split(" ").tail

    handsString.foreach(hs => {
      hands.append(Hand(Board.StringToCards(hs), Seq.empty[Card]))
    })
    hands.foreach(h => h.getHandClassification())
    hands.sortInPlace()(Hand.handSorting)
    hands.foreach(h => println(s"Hand: ${h.cards} , classification: ${h.handClassification}, higher card ${h.cards.sortBy(_.getRank)} pairs: ${h.pairs}"))
    hands.mkString(" ")
  }
}


