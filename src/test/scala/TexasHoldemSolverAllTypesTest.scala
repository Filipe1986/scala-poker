import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class TexasHoldemSolverAllTypesTest extends AnyFunSuite with Matchers {

  test("texas-holdem 5c6dAcAsQs") {
    Solver.process("texas-holdem As2s3s4s9h 5sJs 2hAh QhKh 9hJh 2h2c ") shouldEqual "QhKh 9hJh 2hAh 2h2c 5sJs"
  }
  
}
