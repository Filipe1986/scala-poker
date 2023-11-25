class PokerGameFactory {

  def create(line: String): PokerGame = {
    val split = line.split(" ")
    val pokerGameName = split(0).toLowerCase

    pokerGameName match {
      case "texas-holdem" => new TexasHold(line)
      case "five-card-draw" => new FiveCardDraw(line)
      case "omaha-holdem" => new OmahaHold(line)
      case _ => null // Ideally, you might want to handle this case more gracefully
    }
  }

}