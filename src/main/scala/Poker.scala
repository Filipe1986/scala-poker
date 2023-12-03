object Poker {
  def organizes(line: String): PokerGame = {
    val split = line.split(" ")
    val pokerGameName = split(0).toLowerCase

    pokerGameName match {
      case "texas-holdem" => new TexasHold(line)
      case "five-card-draw" => new FiveCardDraw(line)
      case "omaha-holdem" => new OmahaHold(line)
      case name: String => throw new IllegalArgumentException(s"$name isn't a Poker Game type ")
    }
  }

  object HandValue extends Enumeration {
    val HIGH_CARD, PAIR, TWO_PAIRS, THREE_OF_A_KIND, STRAIGHT, FLUSH, FULL_HOUSE, FOUR_OF_A_KIND, STRAIGHT_FLUSH = Value
  }

  def generateBoard(board: String): Seq[Card] = {
    require(board.length % 2 == 0)
    board.grouped(2).map(s => Card(s)).toSeq
  }

  def stringToCards(cards: String): Seq[Card] = {
    require(cards.length % 2 == 0)
    cards.grouped(2).map(Card.apply).toList
  }

}