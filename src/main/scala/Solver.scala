object Solver {

  def process(line: String): String = {
    val pokerGame = new PokerGameFactory().create(line)
    pokerGame. sortHands()
  }

}
