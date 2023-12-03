import Poker.HandValue
import Poker._
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object Hand {
  implicit val handSorting: Ordering[Hand] = new Ordering[Hand] {
    def compare(x: Hand, y: Hand): Int = {

      if(x.handClassification.compare(y.handClassification) == 0){
        if(x.handClassification.contains(HandValue.HIGH_CARD)){
          compareRank(x, y)
        }else if(x.handClassification.contains(HandValue.PAIR)){

          val compare = x.pairs.head.id.compare(y.pairs.head.id)
          if(compare == 0) {
            compareRank(x, y)
          } else {
            compare
          }
        }else if(x.handClassification.contains(HandValue.TWO_PAIRS)){
          val xPairs = x.pairs.sorted.reverse
          val yPairs = y.pairs.sorted.reverse

          val compare = xPairs.head.id.compare(yPairs.head.id)
          if (compare == 0) {
            val compare2 = xPairs.tail.head.id.compare(yPairs.tail.head.id)
            if (compare2 == 0) {
              compareRank(x, y)
            }else {
              compare2
            }

          } else {
            compare
          }

        } else if(x.handClassification.contains(HandValue.THREE_OF_A_KIND)){
          val compare = x.threeKind.compare(y.threeKind)
          if(compare == 0){
            compareRank(x, y)
          }else {
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
  private var sortedCards: Seq[Card] = Seq.empty[Card]

  private val handValue = Option[HandValue.Value]

  private var flush: Option[Suit.Value] = None
  private var isStraight: Boolean = false
  private var fourKind: Option[Rank.Value] = None
  private var threeKind: Option[Rank.Value] = None
  private var pairs: Seq[Rank.Value] = Seq.empty[Rank.Value]

  private var handClassification: Option[HandValue.Value] = None

  private var suitCounts: Map[Suit.Value, Int] = Map()
  private var rankCounts: Map[Rank.Value, Int] = Map()


  def getHandClassification: HandValue.Value = {
    classifyHand()
    sortedCards = cards.sortBy(_.getRankWeight).reverse
    handClassification = Some(handClassification.getOrElse {
      if (isStraight && flush.nonEmpty) HandValue.STRAIGHT_FLUSH
      else if (fourKind.isDefined) HandValue.FOUR_OF_A_KIND
      else if (threeKind.isDefined && pairs.nonEmpty) HandValue.FULL_HOUSE
      else if (flush.nonEmpty) HandValue.FLUSH
      else if (isStraight) HandValue.STRAIGHT
      else if (threeKind.isDefined) HandValue.THREE_OF_A_KIND
      else if (pairs.length == 2) HandValue.TWO_PAIRS
      else if (pairs.length == 1) HandValue.PAIR
      else HandValue.HIGH_CARD
    })

    handClassification.get
  }

  def getHandWeight: Int = {
    val classification = getHandClassification
    classification.id + 1
  }

   private def classifyHand(): Unit = {


     suitCounts = allCards.groupBy(_.suit).view.mapValues(_.size).toMap
     rankCounts = allCards.groupBy(_.rank).view.mapValues(_.size).toMap

     flush = checkFlush(suitCounts)
     fourKind = rankCounts.collectFirst { case (rank, 4) => rank }
     threeKind = rankCounts.collectFirst { case (rank, 3) => rank }
     pairs = rankCounts.collect { case (rank, 2) => rank }.toSeq
     isStraight = checkStraight
  }


  private def checkFlush(map: Map[Suit.Value, Int]): Option[Suit.Value] = {
    map.find { case (_, count) => count >= 5 }.map(_._1)
  }

  private def checkStraight: Boolean = {
    val sortedCards = allCards.sortBy(_.getRankWeight)
    val isA_highest = sortedCards.last.getRank == Rank.ACE
    val rankWeightsArray: Array[Int] = sortedCards.map(c => c.getRankWeight).toArray
    val newRankWeightsArray = if(isA_highest) 1 +: rankWeightsArray else rankWeightsArray
    newRankWeightsArray.sliding(5).exists(seq => seq.zip(seq.tail).forall { case (a, b) => b == a + 1 })
  }

  override def toString: String = {
    cards.mkString("")
  }

  def printHand(): Unit = {
    println(s"Hand: $cards , classification: $handClassification, higher card ${cards.sortBy(_.getRank)} pairs: $pairs")
  }
}