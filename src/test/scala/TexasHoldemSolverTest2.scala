import Poker.HandValue
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable


class TexasHoldemSolverTest2 extends AnyFunSuite with Matchers {

  val board: String = "2h5c8sAsKc"

  test("Th2h5c8sAsKc3h4c hand classifier") {
    Hand(Poker.stringToCards("3h4c"), Poker.generateBoard(board)).getHandClassification shouldEqual HandValue.STRAIGHT
  }

  test("Th2h5c8sAsKcQs9h hand classifier") {
    Hand(Poker.stringToCards("Qs9h"), Poker.generateBoard(board)).getHandClassification shouldEqual HandValue.HIGH_CARD
  }

  test("Th2h5c8sAsKcJc6s hand classifier") {
    Hand(Poker.stringToCards("Jc6s"), Poker.generateBoard(board)).getHandClassification shouldEqual HandValue.HIGH_CARD
  }

  test("Th2h5c8sAsKc") {
    val board: String = "2h5c8sAsKc"
    Solver.process(s"texas-holdem $board Qs9h Jc6s") shouldEqual "Jc6s Qs9h"
  }

  test("Texas Holdem 2h5c8sAsKc hand classifier") {
    Hand(Poker.stringToCards("2hAh"), Poker.generateBoard(board)).getHandClassification shouldEqual HandValue.TWO_PAIRS
  }


  test("General Test 3") {
    Solver.process("texas-holdem 3d4s5dJsQd 9h7h 2dTc KcAs") shouldEqual "9h7h 2dTc KcAs"
  }

  

}