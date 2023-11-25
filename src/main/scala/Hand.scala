import java.util
import java.util.ArrayList
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object HandValue extends Enumeration {
  val HIGH_CARD, PAIR, TWO_PAIRS, THREE_OF_A_KIND, STRAIGHT, FLUSH, FULL_HOUSE, FOUR_OF_A_KIND, STRAIGHT_FLUSH = Value
}

object Hand {
  implicit val handSorting: Ordering[Hand] = new Ordering[Hand] {
    def compare(x: Hand, y: Hand): Int = {

      if(x.handClassification.compare(y.handClassification) == 0){
        if(x.handClassification == HandValue.HIGH_CARD){
          compareRank(x, y)
        }else if(x.handClassification == HandValue.PAIR){
          val compare = x.pairs.get(0).id.compare(y.pairs.get(0).id)
          if(compare == 0) {
            compareRank(x, y)
          } else {
            compare
          }
        }else {
          println("else 0")
          0
        }
      } else {
        x.handClassification.compare(y.handClassification)
      }

    }

    def compareRank(x: Hand, y: Hand): Int = {


      val xCopy = x.cards.sortBy(_.getRankWeight).reverse
      val yCopy = y.cards.sortBy(_.getRankWeight).reverse

      val handRank = xCopy.zip(yCopy).map { case (a, b) =>
        a.rank.compare(b.rank)
      }

      val result = handRank.filter(_!= 0)

      if(result.nonEmpty) result.head else 0

    }
  }
}



case class Hand(cards: Seq[Card], board: Seq[Card] = Seq.empty[Card]) {

  require(cards.nonEmpty, "Cards cannot be empty")

  private val allCards = cards ++ board

  private val handValue = Option[HandValue.Value]

  private var flush: Option[Suit.Value] = None
  private var isStraight: Boolean = false

  private var fourKind: Rank.Value = null
  private var threeKind: Rank.Value = null
  var pairs: util.ArrayList[Rank.Value] = new util.ArrayList

  var handClassification: HandValue.Value = null

  private var suitCounterMap: mutable.Map[Suit.Value , Int] =  mutable.Map()

  private var rankCounterMap: mutable.Map[Rank.Value, Int] = mutable.Map()


  def getHandClassification(): HandValue.Value = {
    classifyHand()
    if(handClassification != null) {
      return handClassification
    }
    if(isStraight && flush.nonEmpty){
      handClassification = HandValue.STRAIGHT_FLUSH
      return handClassification
    }
    if(fourKind != null){
      handClassification = HandValue.FOUR_OF_A_KIND
      return handClassification
    }
    if(threeKind != null && pairs.size() > 0){
      handClassification = HandValue.FULL_HOUSE
      return handClassification
    }
    if(flush.nonEmpty){
      handClassification = HandValue.FLUSH
      return handClassification
    }
    if(isStraight){
      handClassification = HandValue.STRAIGHT
      return handClassification
    }
    if(threeKind != null){
      handClassification = HandValue.THREE_OF_A_KIND
      return handClassification
    }
    if(pairs.size() == 2){
      handClassification = HandValue.TWO_PAIRS
      return handClassification
    }
    if(pairs.size() == 1){
      handClassification = HandValue.PAIR
      return handClassification
    }
    handClassification = HandValue.HIGH_CARD

    handClassification
  }

  def getHandWeight: Int = {
    val classification = getHandClassification()
    classification.id + 1
  }

  private def handCount(): Unit = {
    suitCounterMap = mutable.Map()
    rankCounterMap = mutable.Map()

    allCards.foreach(c => {
      suitCounterMap(c.suit) = suitCounterMap.getOrElseUpdate(c.suit, 0) + 1
      rankCounterMap(c.rank) = rankCounterMap.getOrElseUpdate(c.rank, 0) + 1
    })
  }

   private def classifyHand(): Unit = {

     handCount()
     checkFlush()
     checkRankCombinations()
     checkStraight()
  }


  private def checkFlush(): Unit = {
    suitCounterMap.foreach { case (suit, count) =>
      if (count >= 5) {
        flush = Some(suit)
      }
    }
  }

  private def checkRankCombinations(): Unit = {
    rankCounterMap.foreach { case (rank, count) =>
      if (count == 4) {
        fourKind = rank
      }
      if (count == 3) {
        threeKind = rank
      }
      if (count == 2){
        pairs.add(rank)
      }
    }
  }

  private def checkStraight(): Unit = {
    val sortedCards = allCards.sortBy(_.getRankWeight)
    val isA_highest = sortedCards.last.getRank == Rank.ACE
    val rankWeightsArray: Array[Int] = sortedCards.map(c => c.getRankWeight).toArray
    val newRankWeightsArray = if(isA_highest) 1 +: rankWeightsArray else rankWeightsArray
    isStraight = newRankWeightsArray.sliding(5).exists(seq => seq.zip(seq.tail).forall { case (a, b) => b == a + 1 })
  }

  override def toString: String = {
    cards.mkString("")
  }



}