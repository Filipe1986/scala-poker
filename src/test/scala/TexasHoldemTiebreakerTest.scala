import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class TexasHoldemTiebreakerTest extends AnyFunSuite with Matchers {

  test("texas-holdem High Card Tie Break") {
    Solver.process("texas-holdem 3d4s5dJsQd 9d8h 9h7h Kh6h") shouldEqual "9h7h 9d8h Kh6h"
  }

  test("texas-holdem One Pair Tie Break") {
    Solver.process("texas-holdem 3d4s5dJsQd Jd8h 9hQh Jh6h") shouldEqual "Jh6h Jd8h 9hQh"
  }

  test("texas-holdem Two Pairs Tie Break") {
    Solver.process("texas-holdem 3d4s5dJsQd Jd5h 4cQh 3hJh") shouldEqual "3hJh Jd5h 4cQh"
  }

  test("texas-holdem second Two Pairs Tie Break") {
    Solver.process("texas-holdem 3d4s5dQsQd KdKh 5c6h 5hJh") shouldEqual "5c6h 5hJh KdKh"
  }

  test("texas-holdem Three of a Kind Tie Break") {
    Solver.process("texas-holdem 3d4s5dQsQd QcKh Qh6h") shouldEqual "Qh6h QcKh"
  }

  test("texas-holdem second Three of a Kind Tie Break") {
    Solver.process("texas-holdem 3d4s5dQsJd QcQh JcJh") shouldEqual "JcJh QcQh"
  }
  
}
