object Suit extends Enumeration {
  val HEARTS, DIAMONDS, CLUBS, SPADES = Value

  def valueOfLabel(label: String): Suit.Value = {
    label.toLowerCase match {
      case "h" => HEARTS
      case "d" => DIAMONDS
      case "c" => CLUBS
      case "s" => SPADES
      case _ => throw new IllegalArgumentException("Unknown suit: " + label)
    }
  }
}

object Rank extends Enumeration {
  val TWO, THREE, FOUR, FIVE, SIX, SEVEN, EIGHT, NINE, TEN, JACK, QUEEN, KING, ACE = Value

  def valueOfLabel(label: String): Rank.Value = {
    label.toUpperCase match {
      case "2" => TWO
      case "3" => THREE
      case "4" => FOUR
      case "5" => FIVE
      case "6" => SIX
      case "7" => SEVEN
      case "8" => EIGHT
      case "9" => NINE
      case "T" => TEN
      case "J" => JACK
      case "Q" => QUEEN
      case "K" => KING
      case "A" => ACE
      case _ => throw new IllegalArgumentException("Unknown rank: " + label)
    }
  }
}

case class Card(card: String) {
  require(card.length == 2, "Card string must be 2 characters long")

  val rank: Rank.Value = Rank.valueOfLabel(card.substring(0, 1))
  val suit: Suit.Value = Suit.valueOfLabel(card.substring(1, 2))

  def getRank: Rank.Value = rank
  def getSuit: Suit.Value = suit
  def getRankWeight: Int = rank.id + 2 // Assuming TWO is the lowest rank

  override def toString: String = {
    card
  }
}
