import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers


class HandClassifierTest extends AnyFunSuite with Matchers {


  test("getHandMissedHandClassification 1") {

    Hand(Board.StringToCards("3d4s5dJsQd9h7h")).getHandClassification() shouldEqual HandValue.HIGH_CARD
    Hand(Board.StringToCards("3d4s5dJsQd2dTc")).getHandClassification() shouldEqual HandValue.HIGH_CARD

  }

  test("getHandMissedHandClassification sdsad") {

    Hand(Board.StringToCards("5s5d7s4dQd")).getHandClassification() shouldEqual HandValue.PAIR
    Hand(Board.StringToCards("7h6h7d2cJc")).getHandClassification() shouldEqual HandValue.PAIR

  }


  test("getHandMissedHandClassification") {

    Hand(Board.StringToCards("5c6dAcAsQs2cJc")).getHandClassification() shouldEqual HandValue.PAIR
    Hand(Board.StringToCards("5c6dAcAsQsKh4h")).getHandClassification() shouldEqual HandValue.PAIR
    Hand(Board.StringToCards("5c6dAcAsQsKs4c")).getHandClassification() shouldEqual HandValue.PAIR
    Hand(Board.StringToCards("5c6dAcAsQsKc7h")).getHandClassification() shouldEqual HandValue.PAIR
    Hand(Board.StringToCards("5c6dAcAsQsKdJs")).getHandClassification() shouldEqual HandValue.PAIR
    
    Hand(Board.StringToCards("5c6dAcAsQs6h7d")).getHandClassification() shouldEqual HandValue.TWO_PAIRS
    Hand(Board.StringToCards("5c6dAcAsQs2hAh")).getHandClassification() shouldEqual HandValue.THREE_OF_A_KIND
  }

  test("getHandClassificationStraightFlush") {
    new Hand(Board.StringToCards("KcQcJcTc9c")).getHandClassification() shouldEqual HandValue.STRAIGHT_FLUSH
    new Hand(Board.StringToCards("QcJcKcTcAc")).getHandClassification() shouldEqual HandValue.STRAIGHT_FLUSH
    new Hand(Board.StringToCards("5s4s2s3sAs")).getHandClassification() shouldEqual HandValue.STRAIGHT_FLUSH
    new Hand(Board.StringToCards("6s2s4s3s5s")).getHandClassification() shouldEqual HandValue.STRAIGHT_FLUSH
  }

  test("classifyFourOfKind") {
    new Hand(Board.StringToCards("4s4h4c4d7c")).getHandClassification() shouldEqual HandValue.FOUR_OF_A_KIND
    new Hand(Board.StringToCards("5s5h7c5c5d")).getHandClassification() shouldEqual HandValue.FOUR_OF_A_KIND
    new Hand(Board.StringToCards("7cAsAhAcAd")).getHandClassification() shouldEqual HandValue.FOUR_OF_A_KIND
    new Hand(Board.StringToCards("5s5h7c5c5d")).getHandClassification() shouldEqual HandValue.FOUR_OF_A_KIND
  }

  test("classifyFullHouse") {
    new Hand(Board.StringToCards("4s4h4c5s5c")).getHandClassification() shouldEqual HandValue.FULL_HOUSE
    new Hand(Board.StringToCards("AsAhJdJsJc")).getHandClassification() shouldEqual HandValue.FULL_HOUSE
  }

  test("classificationFlush") {
    new Hand(Board.StringToCards("4cKc4c8c7c")).getHandClassification() shouldEqual HandValue.FLUSH
    new Hand(Board.StringToCards("2h3h4h5h8h")).getHandClassification() shouldEqual HandValue.FLUSH
  }

  test("getHandClassificationStraight") {
    new Hand(Board.StringToCards("KcQcJcTc9s")).getHandClassification() shouldEqual HandValue.STRAIGHT
    new Hand(Board.StringToCards("AhKcQcJcTc")).getHandClassification() shouldEqual HandValue.STRAIGHT
    new Hand(Board.StringToCards("5h4c3c2cAc")).getHandClassification() shouldEqual HandValue.STRAIGHT
  }

  test("getHandClassificationThreeOfKind") {
    new Hand(Board.StringToCards("KcKcKcTd9s")).getHandClassification() shouldEqual HandValue.THREE_OF_A_KIND
    new Hand(Board.StringToCards("2d2c2dJdTc")).getHandClassification() shouldEqual HandValue.THREE_OF_A_KIND
    new Hand(Board.StringToCards("Jh4d3cJcJc")).getHandClassification() shouldEqual HandValue.THREE_OF_A_KIND
  }

  test("getHandClassificationTwoPairs") {
    new Hand(Board.StringToCards("3cTc3dTd9s")).getHandClassification() shouldEqual HandValue.TWO_PAIRS
    new Hand(Board.StringToCards("AsKdQdKcAc")).getHandClassification() shouldEqual HandValue.TWO_PAIRS
    new Hand(Board.StringToCards("5h4h5c4sAc")).getHandClassification() shouldEqual HandValue.TWO_PAIRS
  }

  test("getHandClassificationPair") {
    new Hand(Board.StringToCards("KcQc3d3h9s")).getHandClassification() shouldEqual HandValue.PAIR
    new Hand(Board.StringToCards("7sKdQhJc7c")).getHandClassification() shouldEqual HandValue.PAIR
    new Hand(Board.StringToCards("5h4h3c2s2d")).getHandClassification() shouldEqual HandValue.PAIR
    new Hand(Board.StringToCards("3d3s5dJsQdKcAs")).getHandClassification() shouldEqual HandValue.PAIR
    new Hand(Board.StringToCards("5s5d7s4dQd")).getHandClassification() shouldEqual HandValue.PAIR
    new Hand(Board.StringToCards("7h6h7d2cJc")).getHandClassification() shouldEqual HandValue.PAIR
    new Hand(Board.StringToCards("5s5d7s4dQd")).getHandClassification() shouldEqual HandValue.PAIR
  }

  test("getHandClassificationHighCard") {
    new Hand(Board.StringToCards("5h4c3c2c7c")).getHandClassification() shouldEqual HandValue.HIGH_CARD
    new Hand(Board.StringToCards("5h4c3c2c7c")).getHandClassification() shouldEqual HandValue.HIGH_CARD
  }
}

