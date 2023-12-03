import scala.language.postfixOps

object Solver {

  def process(gameCards: String): String = {
    val game = Poker organizes gameCards 
    game sortHands
  }

}
