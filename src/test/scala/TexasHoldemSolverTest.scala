import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class TexasHoldemSolverTest extends AnyFunSuite with Matchers {

  test("texas-holdem 5c6dAcAsQs") {
    Solver.process("texas-holdem 5c6dAcAsQs KdJs 2hAh Kh4h Kc7h 6h7d Ks4c 2cJc") shouldEqual
      "2cJc Kh4h Ks4c Kc7h KdJs 6h7d 2hAh"
  }

  test("texas-holdem 2h5c8sAsKc") {
    Solver.process("texas-holdem 2h5c8sAsKc Qs9h KdQh 3cKh Jc6s") shouldEqual "Jc6s Qs9h 3cKh KdQh"
  }

  test("texas-holdem 3d4s5dJsQd") {
    Solver.process("texas-holdem 3d4s5dJsQd 5c4h 7sJd KcAs 9h7h 2dTc Qh8c TsJc") shouldEqual
      "9h7h 2dTc KcAs 7sJd TsJc Qh8c 5c4h"
  }
}
