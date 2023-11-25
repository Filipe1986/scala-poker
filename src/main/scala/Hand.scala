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
          var compare = x.pairs.get(0).id.compare(y.pairs.get(0).id)
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
      var xCopy = x.cards.clone()
      xCopy = xCopy.sortBy(_.getRankWeight).reverse
      var yCopy = y.cards.clone()
      yCopy = yCopy.sortBy(_.getRankWeight).reverse

      val handRank = xCopy.zip(yCopy).map { case (a, b) =>
        a.rank.compare(b.rank)
      }

      var result = handRank.filter(_!= 0)

      if(result.nonEmpty) result.head else 0

    }
  }
}



case class Hand(cards: mutable.ArrayBuffer[Card] = null, board: mutable.ArrayBuffer[Card] = new mutable.ArrayBuffer()) {

  require(cards.nonEmpty, "Cards cannot be empty")

  private val allCards = cards ++ board

  private val handValue = Option[HandValue.Value]

  private var flush: Option[Suit.Value] = None
  private var isStraight: Boolean = false

  private var fourKind: Rank.Value = null
  private var threeKind: Rank.Value = null
  var pairs: util.ArrayList[Rank.Value] = new ArrayList

  var handClassification: HandValue.Value = null

  private var suitCounterMap: mutable.Map[Suit.Value , Int] =  mutable.Map()

  private var rankCounterMap: mutable.Map[Rank.Value, Int] = mutable.Map()


  def getHandClassification(): HandValue.Value = {
    classifyHand
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


    return handClassification
  }

  def getHandWeight: Int = {
    val classification = getHandClassification()
    classification.id + 1
  }

  def handCount: Unit = {
    suitCounterMap = mutable.Map()
    rankCounterMap = mutable.Map()

    allCards.foreach(c => {
      suitCounterMap(c.suit) = suitCounterMap.getOrElseUpdate(c.suit, 0) + 1
      rankCounterMap(c.rank) = rankCounterMap.getOrElseUpdate(c.rank, 0) + 1
    })
  }

   def classifyHand: Unit = {

     handCount

     checkFlush
     checkRankCombinations
     checkStraight
  }


  private def checkFlush: Unit = {
    suitCounterMap.foreach { case (suit, count) =>
      if (count == 5) {
        flush = Some(suit)
      }
    }
  }

  private def checkRankCombinations: Unit = {
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


  def hasSequence(arr: Array[Int], sequenceLength: Int): Unit = {
    this.isStraight = false
    var count = 1
    var i = 0
    for (i <- 0 until arr.length - 1) {
      if (arr(i) + 1 == arr(i + 1)) {
        count += 1
        if (count >= sequenceLength) isStraight = true
      } else {
        count = 1
      }
    }
  }


  def checkStraight: Unit = {
    allCards.sortInPlaceBy(_.getRankWeight)
    val isA_highest = allCards.last.getRank == Rank.ACE
    val rankWeightsArray: Array[Int] = allCards.map(c => c.getRankWeight).toArray
    val newRankWeightsArray = if(isA_highest) 1 +: rankWeightsArray else rankWeightsArray
    hasSequence(newRankWeightsArray, 5)
  }

  override def toString: String = {
    cards.mkString("")
  }



}