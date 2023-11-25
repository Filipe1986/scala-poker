import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class RankAndSuitTest extends AnyFunSuite with Matchers {

  test("classify card") {
    val card2s = Card("2s")
    card2s.getRank shouldEqual Rank.TWO
    card2s.getRankWeight shouldEqual 2
    card2s.getSuit shouldEqual Suit.SPADES

    val card3h = Card("3h")
    card3h.getRank shouldEqual Rank.THREE
    card3h.getRankWeight shouldEqual 3
    card3h.getSuit shouldEqual Suit.HEARTS

    val cardTc = Card("Tc")
    cardTc.getRank shouldEqual Rank.TEN
    cardTc.getRankWeight shouldEqual 10
    cardTc.getSuit shouldEqual Suit.CLUBS

    val cardTd = Card("td")
    cardTd.getRank shouldEqual Rank.TEN
    cardTd.getRankWeight shouldEqual 10
    cardTd.getSuit shouldEqual Suit.DIAMONDS
  }

  test("classify suits") {
    Suit.valueOfLabel("s") shouldEqual Suit.SPADES
    Suit.valueOfLabel("h") shouldEqual Suit.HEARTS
    Suit.valueOfLabel("c") shouldEqual Suit.CLUBS
    Suit.valueOfLabel("d") shouldEqual Suit.DIAMONDS
  }
}
