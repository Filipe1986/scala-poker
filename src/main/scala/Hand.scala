import Poker.HandValue
import Poker._
import java.util
import java.util.ArrayList
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object Hand {
  implicit val handSorting: Ordering[Hand] = new Ordering[Hand] {
    def compare(x: Hand, y: Hand): Int = {

      // TODO missing compare same handClassification for two pairs, three of a kind, straight, flush, four of a kind and straight flush
      if(x.handClassification.compare(y.handClassification) == 0){
        if(x.handClassification.contains(HandValue.HIGH_CARD)){
          compareRank(x, y)
        }else if(x.handClassification.contains(HandValue.PAIR)){
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
  private var fourKind: Option[Rank.Value] = None
  private var threeKind: Option[Rank.Value] = None
  private val pairs: util.ArrayList[Rank.Value] = new util.ArrayList

  var handClassification: Option[HandValue.Value] = None

  private var suitCounts: Map[Suit.Value, Int] = Map()

  private var rankCounts: Map[Rank.Value, Int] = Map()


  def getHandClassification: HandValue.Value = {
    classifyHand()
    if(handClassification.isDefined) {
      return handClassification.get
    }
    if(isStraight && flush.nonEmpty){
      handClassification = Some(HandValue.STRAIGHT_FLUSH)
      return handClassification.get
    }
    if(fourKind.isDefined){
      handClassification = Some(HandValue.FOUR_OF_A_KIND)
      return handClassification.get
    }
    if(threeKind.isDefined && pairs.size() > 0){
      handClassification = Some(HandValue.FULL_HOUSE)
      return handClassification.get
    }
    if(flush.nonEmpty){
      handClassification = Some(HandValue.FLUSH)
      return handClassification.get
    }
    if(isStraight){
      handClassification = Some(HandValue.STRAIGHT)
      return handClassification.get
    }
    if(threeKind.isDefined){
      handClassification = Some(HandValue.THREE_OF_A_KIND)
      return handClassification.get
    }
    if(pairs.size() == 2){
      handClassification = Some(HandValue.TWO_PAIRS)
      return handClassification.get
    }
    if(pairs.size() == 1){
      handClassification = Some(HandValue.PAIR)
      return handClassification.get
    }
    handClassification = Some(HandValue.HIGH_CARD)

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
     checkRankCombinations()
     checkStraight()
  }


  private def checkFlush(map: Map[Suit.Value, Int]): Option[Suit.Value] = {
    map.find { case (_, count) => count >= 5 }.map(_._1)
  }

  private def checkRankCombinations() : Unit = {
    rankCounts.foreach { case (rank, count) =>
      if (count == 4) {
        fourKind = Some(rank)
      }
      if (count == 3) {
        threeKind = Some(rank)
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

  def printHand(): Unit = {
    println(s"Hand: $cards , classification: $handClassification, higher card ${cards.sortBy(_.getRank)} pairs: $pairs")
  }


}